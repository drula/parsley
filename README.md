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
$ export PATH="$PATH:<homepath>/.cabal
```
where `<homepath>` is a path to your home folder directory (`~`).

## Run
```
parsley <inputfile.prl>
```
where `<inputfile.prl>` is a .prl file with a bitstream format description.
`parsley` will generate `inputfile_parser.h` and `inputfile_parser.c` with
the parser header and source files.

## Clean
```
$ cabal clean
```
