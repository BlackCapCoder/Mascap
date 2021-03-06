# Mascap

This is just regular old [Mascarpone](https://github.com/catseye/Mascarpone), but the standard interpreter has more commands out of the box. In particular I aim to implement various languages and improvements to make it suitable for codegolf.

The gimmick of Mascarpone, and indeed Mascap, is that meta-circular interpreters are first class citizens. The only way to do control flow in standard Mascarpone is with higher-order meta-circular interpreters. For more information, please read the Mascarpone language specification by clicking the link above.

Currently Mascap supports a small subset of [Joy](https://en.wikipedia.org/wiki/Joy_(programming_language)). You can push the joy interpreter to the stack with `J`.

```
'a'b
CJ+^
swap..
```

```
'a       Push a to the stack
'b       Push b to the stack
C        Push the mascap interpreter to the stack
J        Push the joy interpreter to the stack
+        Combine the joy and mascap instruction set into a new interpreter
^        Install this interpreter
swap     Swap the two topmost items on the stack ('a and 'b), this is from the Joy instruction set
.        Print (This is from mascarpone)
.        Print
```
