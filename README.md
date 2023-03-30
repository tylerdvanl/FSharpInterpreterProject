# FSharpInterpreterProject

This project was done during a Programming Languages course at California State University, Long Beach.  I worked with a partner, Jazmin Proano.

The short description of the project is that it is a new, turing complete programming language with an interpreter written in F#.  We made extensive use of
F#'s discriminated unions to create programs and functions, which themselves are lists of statements.  The various statements are combinations of expressions and
conditions, and are used to perform changes to the program state.  

Being fairly simple, there are some limitations to the language, mainly in that it only supports floating point numbers and strings as variables.  That is to say, no
objects or user-defined types.
