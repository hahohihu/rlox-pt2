#!/usr/bin/env bash
# Some tests get time, which miri doesn't allow with isolation enabled.
MIRIFLAGS='-Zmiri-disable-isolation' cargo +nightly miri test --no-default-features "$@"
