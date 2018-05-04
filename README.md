# parsley
Bitstream parser generator in C

## Installing build dependencies
Download and install [Haskell platform](https://www.haskell.org/downloads#platform).

Update `cabal` packages
```
$ cabal update
```

## Install
```
$ cabal install
```
Make sure that `~/.cabal` is added to PATH system variable.
```
$ export PATH="$PATH:$HOME/.cabal/bin"
```

## Run
```
parsley <inputfile.prl>
```
where `<inputfile.prl>` is a .prl file with a bitstream format description.
`parsley` will generate `inputfile_parser.h` and `inputfile_parser.c` with
the parser header and source files.

As an example of an input file you can use `implemented.prl`.

## Clean
```
$ cabal clean
```
