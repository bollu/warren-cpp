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

To build, use `cmake` with `make/ninja`:

```
$ mkdir build && cd build && cmake ../ && make
```


```
$ mkdir build && cd build && cmake -Gninja ../ && ninja
```

For autocompletion, I use `YouCompleteMe` along with `cmake`'s ability
to generate autocomplete information as follows:

```
$ cd build && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ../
$ cp compile_commands.json ../
```
