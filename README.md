## Synopsis

The compiler compiles from Fasto to Mips assembly.

This is an attempt to port a compiler for the language Fasto from SML to
Haskell. Small extensions to the language definition has been made such
that it for example is possible to write several declarations within a let
statement.

Two library functions are provided: read and write. They compile to system calls
compatible with the MARS implementation of the MIPS architechture.

## Fasto Code Example

### A recursive factorial function in Fasto
```
fun int fact(int n) =
  if n == 0
  then 1
  else n*fact(n-1)

fun int main() =
  let n = read()
      p = fact(n)
  in write(p)
```

### A recursive Fibonacci function in Fasto
```
fun int fibo(int n) =
  if n == 0 then 0
  else if n == 1 then 1
  else fibo(n - 1) + fibo(n - 2)

fun int main() =
  let n = read()
      p = fibo(n)
  in write(p)
```

## Modules

The compiler consists of the following modules
1. FastoParser.hs
    * Parses the fasto code and generates an abstract syntax tree (AST). The types in the AST are defined in the file Fasto.hs.
2. FastoCodeGenerator.hs
    * Takes the Fasto AST as input and creates another AST as output. The output AST is of an intermediate language type. The syntax of the intermediate language is defined in Imp.hs. It is a subset of the intermediate language defined in Torben Ægidius Mogensen's Introduction to Compiler Design.
3. MipsCodeGenerator.hs
    * Generates the symbolic machine code where an infinite number of registers
are available but where calling conventions are observed. 
4. MipsRegAlloc.hs
    * Translates from MIPS code with symbolic registers to MIPS code with actual 
register names. The register names that in the input start with "$" are not
mapped to real registers since these registers are already real registers which
are set by the MipsCodeGenerator in order to observe the calling conventions.
5. TypeChecker.hs 
    * Not yet being used by Main.hs. Should be inserted between the FastoParser.hs
and the FastoCodeGenerator.hs modules.

## Installation

The program has to be compiled manually using
```
ghc Main.hs
```
And the needed modules should also be installed manually.

## Run the Files

The compiled assembly code can be run using the MARS MIPS Simulater available at
http://courses.missouristate.edu/KenVollmar/MARS/

## License

The project is released under the MIT license. See LICENSE.txt in the root
directory.
