---
title: Assignment 2 - Minzinc-Lite
author: FIT2102 Programming Paradigms
margin: 1inch
---

Please do not change the names of the functions defined in the Assignment.hs file. You may (and are highly encouraged) to implement your parsers **alongside** these pre-defined functions.

## Running the Code

```
$ stack test
```

This will generate the output files using the sample input MiniZinc files, by running your code.

All example MiniZinc files are stored within `examples/inputs` and the output of your parser will be saved in `examples/output`.

## Running the Interactive Page

In the Haskell folder run:

```
$ stack run
```

In a separate terminal, in the javascript folder run:

```
$ npm i
$ npm run dev
```

You can type in to the LHS of the webpage and inspect the generated solution.
