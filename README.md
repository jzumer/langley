An experimental language project.

As an experiment, code quality or correctness weren't goals (euphemism for it's buggy and low quality!)

It uses an expanded though partial version of the destination-driven code generation paradigm, which results in some pretty nice optimizations in codegen. However, I think that unlike the original paper's claims, peepholes might be a better idea overall in terms of gains vs complexity + effort.

An example fizzbuzz is provided in the `example.l` file. The compiler is a repl'd JIT compiler that outputs machine code directly. x86_64 is the only supported target.

To run the example, use

```
make
./main
load ( "example.l" ) ;
```

Note that the tokenizer is (almost) a simple split-at-spaces function, so each element (name, numbers, strings except quotes, parentheses, etc.) must be separated with spaces.
