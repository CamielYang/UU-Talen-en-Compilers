# Analysis Documentation

This document will contain all analysis that are being donee during compilation.

## Scope Analysis

* Check if variable is in scope.
* Check if Function is in scope. Functions can be called before it is declared. The analysis uses two phases to make this possible. One to fill up the initial symbol table, which contains all functions.

## Type Analysis

* Check if functions have the right amount of arguments.
* Check if the given arguments are correct.
* Check if any assignment to variables are of the correct type.
* Check if the return value matches the return of the method.
* Check if all return values matches. It doesn't work perfectly, because it only check the list of returns that are given in the function. It doesn't handle cases wether all code paths return a value.
* Some code use head to take the first returntype (like the fExprOp function). This is okay, since all return types will be handled in another function.
