`D` Vanilla Mascarpone interpreter
----------------------------------

`v` ("reify") pushes the current interpreter onto the stack.

`^` ("deify") pops an interpreter from the stack and installs it as the
current interpreter.

`>` ("extract") pops a symbol from the stack, then pops an interpreter.
It pushes onto the stack the operation associated with that symbol in
that interpreter.

`<` ("install") pops a symbol from the stack, then an operation, then an
interpreter. It pushes onto the stack a new interpreter which is the
same as the given interpreter, except that in it, the given symbol is
associated with the given operation.

`{` ("get parent") pops an interpreter from the stack and pushes it's
parent interpreter onto the stack.

`}` ("set parent") pops an interpreter i from the stack, then pops an
interpreter j. It pushes a new interpreter which is the same as i,
except that it's parent interpreter is j.

`*` ("create") pops an interpreter from the stack, then a string. It
creates a new operation defined by how that interpreter would interpret
that string of symbols, and pushes that operation onto the stack.

`@` ("expand") pops an operation from the stack and pushes a program
string, then pushes an interpreter, such that the semantics of running
the program string with the interpreter is identical to the semantics of
executing the operation. (Note that the program need not be the one that
the operation was defined with, only *equivalent* to it, under the given
interpreter; this allows one to sensibly expand "intrinsic" operations
like those in the initial Mascarpone interpreter.)

`!` ("perform") pops an operation from the stack and executes it.

`0` ("null") pushes the null interpreter onto the stack.

`1` ("uniform") pops an operation from the stack and pushes back an
interpreter where all symbols are associated with that operation.

`[` ("deepquote") pushes a `[` symbol onto the stack and enters "nested
quote mode", which is really another interpreter. In nested quote mode,
each symbol is interpreted as an operation which pushes that symbol onto
the stack. In addition, the symbols `[` and `]` have special additional
meaning: they nest. When a `]` matching the first `[` is encountered,
nested quote mode ends, returning to the interpreter previously in
effect.

`'` ("quotesym") switches to "single-symbol quote mode", which is really
yet another interpreter. In single-symbol quote mode, each symbol is
interpreted as an operation which pushes that symbol onto the stack,
then immediately ends single-symbol quote mode, returning to the
interpreter previously in effect.

`.` pops a symbol off the stack and sends it to the standard output.

`,` waits for a symbol to arrive on standard input, and pushes it onto
the stack.

`:` duplicates the top element of the stack.

`$` pops the top element of the stack and discards it.

`/` swaps to the top two elements of the stack.


`C` Mascap interpreter
-----------------------------------

This is the default interpreter. It has all of the vanilla Mascarpone operations, as well as:


`C` pushes the Mascap interpreter onto the stack

`D` pushes the Mascarpone interpreter onto the stack

`J` pushes the Joy interpreter onto the stack

`+` pops two interpreters from the top of the stack and pushes back a new interpreter that has all of the operations from the original two


`J` Joy interpreter
-------------------

Please see [this document](http://www.kevinalbrecht.com/code/joy-mirror/plain-manual.html)
