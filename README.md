# MRJP-Instant-Compiler

## Description
You can read detailed description in either [polish](Instant.md) or [english](Instant-en.md).

## Usage
To build the project run
```
make
```

For LLVM run
```bash
./insc_llvm foo/bar/baz.ins
lli foo/bar/baz.bc
```

For JVM run
```bash
./insc_jvm foo/bar/baz.ins
java foo.bar.baz
```

You can also provide a directory as an argument and both compilers would compile all `.ins` files in there.

## External libraries
- [Jasmin](https://jasmin.sourceforge.net/)
- [BNFC](https://bnfc.digitalgrammars.com/)

## Project directory structure
- [lib](lib) - contains [jasmin.jar](lib/jasmin.jar)
- [src](src) - project's root directory
    - [Instant](src/Instant/) - automatically generated with BNFC, based on [grammar](src/Instant.cf)
    - [LLVMCompiler](src/LLVMCompiler.hs), [LLVMMain](src/LLVMMain.hs) - the code of the compiler to LLVM
    - [JVMCompiler](src/JVMCompiler.hs), [JVMMain](src/JVMMain.hs) - the code of the compiler to JVM
    - [Makefile](src/Makefile) - builds the project, it is used by the main [Makefile](Makefile)

## Examples
You can run both compilers on [example programs](instant231015/examples/) using bash [script](script.sh):
```bash
./script
./script exec
```
