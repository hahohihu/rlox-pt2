This is an implementation of clox that sticks pretty close to the [book](https://craftinginterpreters.com).

# Safety and Optimization

I intended to mostly stick to safe Rust early on (except for GC-managed pointers), but benchmarks showed my implementation performing similarly to jlox. That was mostly because of bounds checks, conditions, etc. in tight loops that presumably inhibited compiler optimization (I don't think the branch alone would account for a 3x difference with branch prediction). 

The clox VM is deeply unsafe, but is sound given the way codegen happens. For example, pushing and popping off the stack were bottlenecks due to bounds checking, but both never actually need to be bounds checked - even with a fixed array. Popping isn't a huge surprise, since the codegen will only pop at the end of a scope, statement, etc. Pushing works out because there is are upper limits on _everything_. There can only be `u8::MAX` variables, and there's a maximum recursion limit. So we can elide bounds checks entirely - that alone got me 1/2 the way to clox.

Other noteworthy optimizations that got me pretty close to clox:
- Removing bounds checking reading the next opcode, getting the callframe, etc.
- Making reading invalid opcodes UB

# Pain points

## Closures

The clox implementation of closures is extremely Rust-unfriendly, and for once, self-referential data was not the worst thing to deal with. The bigger issue was that captured variables get a pointer into the stack, which makes for painful aliasing problems. Using UnsafeCell fixed at least part of the problem, but the general aliasing problem remains. A `VM` has ownership over a `Stack`, so even though it can only call functions that take a `&Stack`, dereferencing pointers to the stack carries the risk of invalidating any `&mut VM` taken. The current implementation has UB under stack borrows, though seems temporarily fine under tree borrows. There are workarounds to make it work under stack borrows, but they're all hackjobs or make the code unbearably ugly.

## Single-pass compilation

Parsing isn't the bottleneck, and I found doing everything in one pass made the code significantly more difficult to reason about, so I added an AST.

# Where I deviated from the book

I've skipped some segments that weren't interesting to me - notably:
- Implementing dynamic arrays and hashmaps
- Chapters 27-29 - which didn't seem like they taught anything new
- Chapter 30 - I don't use hashing at runtime, and NaN-boxing seems like the gains are too marginal for how much Rust would dislike it

Additionally, I deviate functionally in a few areas:
- Top level returns are OK
- Shadowing is OK
- Curly braces after an if-else are mandatory
- Parens around an if-condition are optional

# Neat tooling that was helpful sniffing out bugs

I benefited from a lot of other tooling, but these are the ones that are new to _me_:

[Tarpaulin](https://github.com/xd009642/tarpaulin) is a code coverage tool - it really did force me to find edge cases I hadn't considered.

[cargo-fuzz](https://github.com/rust-fuzz/cargo-fuzz) is a CLI helper for fuzzing - it took a while to set up and mostly found false-negatives (pre-conditions in the AST generation that weren't adequately specified), but it did find a buffer overflow. After switching to a 2-pass compilation process, I optimized if-else to omit some code if there was no else. However, I didn't emit a `Pop` on one of the branches in that case, so the stack grew and grew until it overflowed. By that point, I'd already optimized with the assumption that could never happen, so I'm glad for fuzzing + asan. Another approach that would've helped is intrusive testing to check the stack, but I've had bad experiences with that - it makes changing implementation details difficult.

[insta](https://insta.rs/) is a snapshot testing tool. It's particularly great for UI testing - where I basically just wanted to say "looks good to me", without actually going through the effort of specifying the result. I wound up using it for basically everything for convenience, and it certainly helped to encourage me to write as many tests as I did. When I implemented jlox, overly verbose tests were a huge pain point that I've learned from. A downside is that they seem to encourage laziness. Often enough, I would write out the input, skim over the result (particularly for the AST or bytecode) and accept it when it was actually wrong.
