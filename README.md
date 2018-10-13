# yoga-hs
Haskell bindings to [Facebook's Yoga layout library](https://facebook.github.io/yoga/).
------

These bindings run roushshod over cabal's best practices for including C
sources. Yoga is mostly implemented in C++, with a C-based header file used for
interfacing with other languages. Since we include the source, most of the files
that are passed to the C compiler are in fact C++. With `gcc`, this isn't too
big a problem, and most distributions of `ghc` come with their own `gcc`.
However, on certain platforms like OS X, the version of `gcc` is much older than
what is able to discern usage of `--std=c++11` between C and C++ files. While
all of the sources for `Yoga` are C++, `hsc2hs` generates a single C file that
is used as an interface between the two. This means that we're mixing C++ and C
files, and we need a compiler that can take a common set of flags for both. For
that reason, if you use this library, we suggest that you install a recent
version of `gcc` to use with `ghc`. Version 7+ seems to work.
