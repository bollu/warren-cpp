# Warren
An implementation of the 
[warren abstract machine](http://wambook.sourceforge.net/wambook.pdf) to learn
about efficiently compiling logic programs.

The eventual hope is to move this project to haskell, where we can use
interesting concurrency primitives in an attempt to parallelise the
computation.

This is why all functions in this project are pure functions, and perorm no
state mutatiaon. At some point (once we have backtracking), it would be
interesting to hook in a green threads library and parallelise the computation
across threads, which should be "stupid easy" due to zero global state.

## Reading material
- [The warren abstract machine, A tutorial reconstruction](warren-abstract-machine-prolog.pdf)
- [Datalog lecture notes](https://www.cs.cmu.edu/~fp/courses/lp/lectures/26-datalog.pdf)
- [Logic programming course@CMU](https://www.cs.cmu.edu/~fp/courses/lp/)

## Building from source

Recommend setting up the `post-commit` hook so the docs auto-build:

```
$ (at root of repo)
$ ln -s $(pwd)/post-commit .git/hooks/post-commit
```

To build, simply use `cabal`:
```
cabal v2-build
```
