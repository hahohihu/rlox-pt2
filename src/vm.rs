mod stack;
pub mod upvalue;
use std::{io::Write, mem::size_of, ops::Range};

use ariadne::{Color, Label, Report, ReportKind, Source};
use bytemuck::{pod_read_unaligned, AnyBitPattern};

use crate::{
    bytecode::chunk::{Chunk, OpCode},
    common::{
        try_as::{TryAs, TryCast},
        ui::{self, Span},
    },
    compiler::compile,
    value::{
        function::{ObjClosure, ObjFunction},
        native_function::{CallError, NativeFunction},
        object::{Object, ObjectKind},
        string::UnsafeString,
        valid::ValidPtr,
        Value,
    },
};

use self::{stack::FixedStack, upvalue::Upvalue};

#[derive(Copy, Clone, Debug)]
struct CallFrame {
    base_pointer: usize,
    return_addr: usize,
    closure: ObjClosure,
}

struct VM<'src, Stderr, Stdout> {
    chunk: Chunk,
    ip: usize,
    callframe: Vec<CallFrame>,
    stack: FixedStack,
    source: &'src str,
    stderr: Stderr,
    stdout: Stdout,
    /// SAFETY INVARIANT: All objects in objects are valid, and there are no duplicate allocations
    /// This is used to look for inaccessible objects to free
    objects: Vec<Object>,
    upvalue_storage: Vec<ValidPtr<Upvalue>>,
    /// Chunk is the source of truth for indices
    globals: Vec<Option<Value>>,
    open_upvalues: Option<ValidPtr<Upvalue>>,
    next_gc: usize,
}

impl<'src, Stderr, Stdout> Drop for VM<'src, Stderr, Stdout> {
    fn drop(&mut self) {
        debug_assert!(self.open_upvalues.is_none());
        for object in &self.objects {
            unsafe {
                // SAFETY: See safety invariant on objects
                object.free();
            }
        }

        for upvalue in &self.upvalue_storage {
            unsafe {
                // no internals to free
                ValidPtr::free(*upvalue);
            }
        }
    }
}

#[derive(Debug)]
pub enum InterpretError {
    CompileError = 1,
    RuntimeError = 2,
}

type InterpretResult = Result<(), InterpretError>;

impl<'src, Stderr: Write, Stdout: Write> VM<'src, Stderr, Stdout> {
    fn new(chunk: Chunk, source: &'src str, stderr: Stderr, stdout: Stdout) -> Self {
        debug_assert!(!chunk.instructions.is_empty());
        Self {
            callframe: vec![],
            ip: 0,
            chunk,
            source,
            stack: FixedStack::new(),
            objects: vec![],
            upvalue_storage: vec![],
            stderr,
            stdout,
            globals: vec![],
            open_upvalues: None,
            next_gc: 1024,
        }
    }

    fn next_byte(&mut self) -> u8 {
        let byte = self.chunk.instructions[self.ip];
        self.jump(1);
        byte
    }

    /// This gets the span over the range relative to the current IP
    /// Note that this is a bit wonky since instructions aren't in the same order as code
    /// (and are variable-length)
    fn get_span(&self, range: Range<isize>) -> Span {
        let start = self.ip_offset() as isize + range.start;
        let end = self.ip_offset() as isize + range.end;
        debug_assert!(start < end);
        if start < 0 {
            // The bytecode will always have at least a return
            return self.chunk.spans[0];
        }
        let range = Range {
            start: start.max(0) as usize,
            end: (end as usize).min(self.chunk.spans.len()),
        };
        Span::unite_many(&self.chunk.spans[range])
    }

    fn read_constant(&mut self) -> Value {
        let i = self.next_byte();
        self.chunk.get_constant(i)
    }

    fn binary_num_op(&mut self, name: &str, op: impl Fn(f64, f64) -> Value) -> InterpretResult {
        let b = self.pop();
        let a = self.pop();
        match (a, b) {
            (Value::Num(a), Value::Num(b)) => self.push(op(a, b))?,
            (a, b) => {
                let span = self.get_span(-2..1);
                self.runtime_error(
                    span,
                    format!(
                        "Operator '{name}' takes two numbers. Got a {} ({a}) and a {} ({b}).",
                        a.typename(),
                        b.typename()
                    ),
                );
                return Err(InterpretError::RuntimeError);
            }
        }
        Ok(())
    }

    fn runtime_error(&mut self, span: Span, message: String) -> InterpretError {
        Report::build(ReportKind::Error, (), ui::OFFSET)
            .with_message(message)
            .with_label(Label::new(span).with_color(Color::Red))
            .finish()
            // this mutable borrow infects everything it touches, hence &mut self
            // it isn't currently presenting an issue, but perhaps DI was a mistake
            .write(Source::from(self.source), &mut self.stderr)
            .unwrap();
        InterpretError::RuntimeError
    }

    fn define_global(&mut self, index: u8, value: Value) {
        let index = index as usize;
        while self.globals.len() <= index {
            self.globals.push(None);
        }
        self.globals[index] = Some(value);
    }

    fn get_global(&mut self, index: u8) -> Result<Value, InterpretError> {
        match self.globals.get(index as usize) {
            Some(Some(value)) => Ok(*value),
            _ => {
                let span = self.get_span(-2..0);
                self.runtime_error(
                    span,
                    format!("Undefined variable: {}", self.chunk.globals.get_name(index)),
                );
                Err(InterpretError::RuntimeError)
            }
        }
    }

    fn set_global(&mut self, index: u8, value: Value) -> InterpretResult {
        match self.globals.get_mut(index as usize) {
            Some(v) if v.is_some() => {
                *v = Some(value);
                Ok(())
            }
            _ => {
                let span = self.get_span(-2..0);
                self.runtime_error(
                    span,
                    format!("Undefined variable: {}", self.chunk.globals.get_name(index)),
                );
                Err(InterpretError::RuntimeError)
            }
        }
    }

    fn negate(&mut self) -> InterpretResult {
        let val = self.pop();
        match val {
            Value::Num(n) => {
                self.push(Value::Num(-n))?;
            }
            val => {
                let span = self.get_span(-1..0);
                self.runtime_error(
                    span,
                    format!("Tried to negate a {} ({val})", val.typename()),
                );
                return Err(InterpretError::RuntimeError);
            }
        }
        Ok(())
    }

    fn add(&mut self) -> InterpretResult {
        let b = self.pop();
        let a = self.pop();
        match (a, b) {
            (Value::Num(a), Value::Num(b)) => {
                self.push(Value::Num(a + b))?;
                return Ok(());
            }
            (Value::Object(a), Value::Object(b)) => {
                let a = UnsafeString::try_cast(a);
                let b = UnsafeString::try_cast(b);
                if let (Some(a), Some(b)) = (a, b) {
                    let concatenated = Object::from(a + b);
                    self.objects.push(concatenated);
                    self.push(Value::Object(concatenated))?;
                    return Ok(());
                }
            }
            _ => {}
        }
        let span = self.get_span(-2..1);
        self.runtime_error(
            span,
            format!(
                "Operator '+' takes two numbers. Got a {} ({a}) and a {} ({b}).",
                a.typename(),
                b.typename()
            ),
        );
        Err(InterpretError::RuntimeError)
    }

    fn jump(&mut self, offset: isize) {
        self.ip = (self.ip as isize + offset) as usize;
    }

    fn read<T: AnyBitPattern>(&mut self) -> T {
        let size = size_of::<T>();
        let bytes = &self.chunk.instructions[self.ip..][..size];
        self.ip += size;
        pod_read_unaligned(bytes)
    }

    fn ip_offset(&self) -> usize {
        self.ip
    }

    fn base_pointer(&self) -> usize {
        self.callframe
            .last()
            .map(|frame| frame.base_pointer)
            .unwrap_or(0)
    }

    #[cfg(feature = "verbose_vm")]
    fn show_debug_trace(&self) {
        self.chunk
            .disassemble_instruction(self.ip_offset(), self.source, std::io::stdout());
        println!("==== STACK ====");
        for value in unsafe { self.stack.slice() } {
            println!("{value}");
        }
        println!("==== OPEN UPVALUES ====");
        let mut it = self.open_upvalues;
        while let Some(upvalue) = it {
            println!("{}", *upvalue.value);
            it = upvalue.next_open;
        }
        println!("==== GLOBALS ====");
        for (i, v) in self.globals.iter().enumerate() {
            if let Some(v) = v {
                println!("{} = {}", self.chunk.globals.get_name(i as u8), v);
            }
        }
        println!("=================");
    }

    fn peek(&self, i: usize) -> Value {
        self.stack.peek(i).unwrap()
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn push(&mut self, value: Value) -> InterpretResult {
        if self.stack.push(value) {
            Ok(())
        } else {
            self.runtime_error(
                self.get_span(-1..0),
                format!("Overflowed the stack trying to push {}", value),
            );
            Err(InterpretError::RuntimeError)
        }
    }

    fn function_call(&mut self, closure: ObjClosure, arg_count: u8) -> InterpretResult {
        let function = closure.function;
        if function.arity != arg_count {
            let span = self.get_span(-2..0);
            self.runtime_error(
                span,
                format!(
                    "Function {} expects {} arguments, but got {}",
                    function.name, function.arity, arg_count
                ),
            );
            return Err(InterpretError::RuntimeError);
        }
        self.callframe.push(CallFrame {
            // -1 to include function itself
            base_pointer: self.stack.len() - arg_count as usize - 1,
            return_addr: self.ip,
            closure,
        });
        self.ip = function.addr;
        Ok(())
    }

    fn native_function_call(&mut self, function: NativeFunction, arg_count: u8) -> InterpretResult {
        match function.call(unsafe { &self.stack.slice()[self.stack.len() - arg_count as usize..] })
        {
            Ok(value) => {
                self.push(value)?;
                Ok(())
            }
            Err(CallError::ArityMismatch(arity)) => {
                let span = self.get_span(-2..0);
                Err(self.runtime_error(
                    span,
                    format!(
                        "Function {} expects {} arguments, but got {}",
                        function.name, arity, arg_count
                    ),
                ))
            }
            Err(CallError::TypeMismatch(index, expected)) => {
                let span = self.get_span(-2..0);
                Err(self.runtime_error(span, format!("Argument {} expected {}", index, expected)))
            }
        }
    }

    fn call(&mut self, arg_count: u8) -> InterpretResult {
        let value = self.peek(arg_count.into());
        match value.try_as() {
            Some(ObjectKind::Closure { fun }) => self.function_call(fun, arg_count),
            Some(ObjectKind::NativeFunction { fun }) => self.native_function_call(fun, arg_count),
            _ => {
                let span = self.get_span(-2..0);
                self.runtime_error(
                    span,
                    format!("Canot call a value of type {}", value.typename()),
                );
                Err(InterpretError::RuntimeError)
            }
        }
    }

    fn capture_upvalue(&mut self, value: ValidPtr<Value>) -> ValidPtr<Upvalue> {
        let mut prev = None;
        let mut current = self.open_upvalues;
        while let Some(upvalue) = current {
            if upvalue.value.as_ptr() <= value.as_ptr() {
                break;
            }
            prev = current;
            current = upvalue.next_open;
        }

        if let Some(upvalue) = current {
            if upvalue.value.as_ptr() == value.as_ptr() {
                return upvalue;
            }
        }

        let new_upvalue = ValidPtr::new(Upvalue {
            value,
            closed: Value::Num(f64::MAX),
            marked: false,
            next_open: current,
        });
        if let Some(prev) = prev {
            unsafe {
                (*prev.as_ptr()).next_open = Some(new_upvalue);
            }
        } else {
            self.open_upvalues = Some(new_upvalue);
        }
        new_upvalue
    }

    fn close_top_upvalue(&mut self) {
        // this doesn't take a parameter because:
        //   a: it's wrong it only pop a single one off - when a function returns and when it closes upvalues, it must be the top value
        //   b: stacked borrows make &mut awkward to deal with
        let value = unsafe { ValidPtr::from_ptr(self.stack.get_ptr(self.stack.len() - 1)) };
        while let Some(upvalue) = self.open_upvalues {
            if upvalue.value.as_ptr() < value.as_ptr() {
                break;
            }
            unsafe {
                (*upvalue.as_ptr()).closed = *value;
                (*upvalue.as_ptr()).value = ValidPtr::from_ptr(&mut (*upvalue.as_ptr()).closed);
            }
            self.upvalue_storage.push(upvalue);
            self.open_upvalues = upvalue.next_open;
        }
    }

    fn mark_stack(&mut self) {
        let stack = unsafe {
            // SAFETY: values on the stack won't be directly modified, except potentially through their interior pointers
            self.stack.slice()
        };
        for value in stack {
            value.mark();
        }
    }

    fn mark_globals(&mut self) {
        for global in self.globals.iter().flatten() {
            global.mark();
        }
    }

    fn mark_everything(&mut self) {
        self.mark_stack();
        self.mark_globals();

        for frame in self.callframe.iter() {
            frame.closure.mark();
        }

        let mut it = self.open_upvalues;
        while let Some(upvalue) = it {
            // same issue as closures
            unsafe {
                (*upvalue.as_ptr()).marked = true;
            }
            it = upvalue.next_open;
        }
    }

    fn sweep(&mut self) {
        self.objects.retain(|obj| {
            if obj.inner.marked {
                true
            } else {
                unsafe {
                    obj.free();
                }
                false
            }
        });
        self.upvalue_storage.retain(|upval| {
            if upval.marked {
                true
            } else {
                unsafe {
                    // upvalues don't actually store anything that needs to be freed
                    ValidPtr::free(*upval);
                }
                false
            }
        })
    }

    fn allocations(&self) -> usize {
        self.objects.len() + self.upvalue_storage.len()
    }

    fn collect_garbage(&mut self) {
        #[cfg(feature = "verbose_gc")]
        {
            eprintln!(">> GC begin");
        }
        let init = self.allocations();
        #[cfg(not(feature = "stress_gc"))]
        if init < self.next_gc {
            return;
        }
        self.mark_everything();
        self.sweep();
        let new = self.allocations();
        let gc_heap_grow_factor = 2;
        self.next_gc = new * gc_heap_grow_factor;
        #[cfg(feature = "verbose_gc")]
        {
            eprintln!("<< GC end {} -> {}", init, new);
        }
    }

    fn run(&mut self) -> InterpretResult {
        if self.chunk.instructions.is_empty() {
            return Ok(());
        }

        for i in 0..self.chunk.native_globals.len() {
            let (id, value) = self.chunk.native_globals[i];
            self.define_global(id, value);
        }

        loop {
            #[cfg(feature = "verbose_vm")]
            {
                self.show_debug_trace();
            }
            self.collect_garbage();
            let base_pointer = self.base_pointer();
            let instruction: OpCode = self.next_byte().into();
            match instruction {
                OpCode::Return => {
                    let Some(callframe) = self.callframe.pop() else {
                        return Ok(());
                    };
                    let res = self.pop();
                    while self.stack.len() > callframe.base_pointer {
                        self.close_top_upvalue();
                        self.pop();
                    }
                    self.push(res)?;
                    self.ip = callframe.return_addr;
                }
                OpCode::Closure => {
                    let function: ObjFunction = self.read_constant().unwrap_as();
                    let mut upvalues = vec![];
                    for _ in 0..function.upvalues {
                        let local = self.next_byte() != 0;
                        let index = self.next_byte();
                        if local {
                            let stack_value = unsafe { self.stack.get_ptr(index as usize) };
                            let ptr = unsafe { ValidPtr::from_ptr(stack_value) };
                            upvalues.push(self.capture_upvalue(ptr));
                        } else {
                            let outer = self.callframe.last().unwrap().closure;
                            upvalues.push((&*outer.upvalues)[index as usize]);
                        }
                    }
                    let upvalues = ValidPtr::from(upvalues.into_boxed_slice());
                    let closure = Object::from(ObjClosure { function, upvalues });
                    self.objects.push(closure);
                    self.push(Value::from(closure))?;
                }
                OpCode::CloseUpvalue => {
                    self.close_top_upvalue();
                    self.pop();
                }
                OpCode::Call => {
                    let arg_count = self.next_byte();
                    self.call(arg_count)?;
                }
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::JumpRelIfFalse => {
                    let offset = self.read::<u16>();
                    if self.peek(0).falsey() {
                        self.jump(offset as isize);
                    }
                }
                OpCode::JumpRelIfTrue => {
                    let offset = self.read::<u16>();
                    if !self.peek(0).falsey() {
                        self.jump(offset as isize);
                    }
                }
                OpCode::JumpRel => {
                    let offset = self.read::<u16>();
                    self.jump(offset as isize);
                }
                OpCode::Loop => {
                    let offset = self.read::<u16>();
                    self.jump(-(offset as isize));
                }
                OpCode::DefineGlobal => {
                    let index = self.next_byte();
                    let value = self.peek(0);
                    self.define_global(index, value);
                    self.pop();
                }
                OpCode::GetGlobal => {
                    let index = self.next_byte();
                    let value = self.get_global(index)?;
                    self.push(value)?;
                }
                OpCode::SetGlobal => {
                    let index = self.next_byte();
                    let value = self.peek(0);
                    self.set_global(index, value)?;
                }
                OpCode::SetLocal => {
                    let slot = self.next_byte();
                    unsafe {
                        *self.stack.get_ptr(base_pointer + slot as usize) = self.peek(0);
                    }
                }
                OpCode::GetLocal => {
                    let slot = self.next_byte();
                    self.push(self.stack.get(base_pointer + slot as usize).unwrap())?;
                }
                OpCode::GetUpvalue => {
                    let slot = self.next_byte();
                    let closure = self.callframe.last().unwrap().closure;
                    let value = closure.upvalues[slot as usize];
                    self.push(*value.value)?;
                }
                OpCode::SetUpvalue => {
                    let slot = self.next_byte();
                    let closure = self.callframe.last().unwrap().closure;
                    unsafe {
                        let upval = (*closure.upvalues.as_ptr())[slot as usize];
                        (*upval.value.as_ptr()) = self.peek(0);
                    }
                }
                OpCode::Constant => {
                    let constant = self.read_constant();
                    self.push(constant)?;
                }
                OpCode::Nil => {
                    self.push(Value::Nil)?;
                }
                OpCode::True => self.push(Value::Bool(true))?,
                OpCode::False => self.push(Value::Bool(false))?,
                OpCode::Negate => self.negate()?,
                OpCode::Not => {
                    let value = Value::Bool(self.pop().falsey());
                    self.push(value)?;
                }
                OpCode::Print => {
                    let value = self.pop();
                    writeln!(self.stdout, "{value}").unwrap();
                }
                OpCode::Add => self.add()?,
                OpCode::Sub => self.binary_num_op("-", |a, b| Value::Num(a - b))?,
                OpCode::Mul => self.binary_num_op("*", |a, b| Value::Num(a * b))?,
                OpCode::Div => self.binary_num_op("/", |a, b| Value::Num(a / b))?,
                OpCode::Less => self.binary_num_op("<", |a, b| Value::Bool(a < b))?,
                OpCode::Greater => self.binary_num_op(">", |a, b| Value::Bool(a > b))?,
                OpCode::Equal => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Bool(a == b))?;
                }
                OpCode::Invalid => {
                    unreachable!("Reached invalid opcode at {}", self.ip_offset())
                }
            }
        }
    }
}

pub fn interpret(source: &str, mut stderr: impl Write, mut stdout: impl Write) -> InterpretResult {
    let Some(chunk) = compile(source, &mut stderr) else {
        return Err(InterpretError::CompileError);
    };
    let mut vm = VM::new(chunk, source, &mut stderr, &mut stdout);
    vm.run()
}
