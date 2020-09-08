# Rackette
Racket interpreter written in ReasonML


Instructions on how a user would interact with our program:

Unfortunately, VSCode doesn't allow our code to be used interactively by a user. However,
if the user wanted to use our program through an interface such as sketch.sh, it's very 
self-explanatory and convenient to use.

The user only needs to access the rackette procedure. They'll call rackette on a rawProgram, which
is just a string containing a valid Rackette program (which is a language very similar to Racket).
In turn, they could expect as an output a list of strings that would be the outcome of 
disparate individual pieces of their code being properly evaluated (to differentiate it from 
nested code, which would give one output) under the syntax requirements and rules of evaluation
listed for Rackette in the instruction manual. 

An overview of how all the pieces of your program fit together: 

The user will input a string of valid Rackette code which is a rawProgram. This will be taken 
by the rackette procedure, which uses read (defined in Read.re) on each piece of the rawProgram 
input to yield a concreteProgram, which is a list of concreteProgramPieces.

Then, parse maps the parsePiece procedure over all of the concreteProgram, which means that parsePiece
is applied to each concreteProgramPiece.

For each of these concreteProgramPieces, it either goes to parseExpression or parseDefinition depending
on if the concreteProgramPieces start with Symbol("define") or not. parseExpression and parseDefinition
are applied to each of the concreteProgramPieces, which now turns our concreteProgram into an 
abstractProgram full of abstractProgramPieces.

Then, the abstractProgram is processed by the process procedure. 

If it is in fact a definition,
addDefinition is called, which will add a binding ([name, value] to the top level experiment unless
a binding with the same name is already present.

If the abstractProgramPiece is an expression, process will call the eval procedure, which will
turn the expression into a value. 

Finally, rackette will map the stringOfValue procedure across the list of values that was output
by eval performed on all the expressions inside rawProgram.

A list of possible bugs or problems with your program: 

We struggle to think of any bugs
or problems that could arise with the functionality. Each procedure that we wrote was teseted 
extremely thoroughly with checkExpects; in addition, many of the procedures are tested implicitly
through others, especially when we ran rackette on the examples from the rackette instructions.
The most likely issues we can think of are possible variations of invalid code input by the user
that we didn't account for, and don't give a proper, informative failwith but instead lead to a 
ReasonML error.

We believe the warnings we get with are moot/redundant since they've already been eliminated and prevented
from occurring in earlier procedures that the current procedure is dependent upon.

There are likely ways for the user to try to break our code, such as dividing by zero (ReasonML would give a 
suitable error) or by inputting numbers and data that are too large to handle, but these don't seem
avoidable, and our setup code has the overflow exceptions.

List of people I collaborated with: Ziwen Zhou

Extra Features: We did not implement any extra features on our Rackette project
