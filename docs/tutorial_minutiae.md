# Tutorial: Minutiae

In this final tutorial you will learn obscure features and how to calculate binary nibble code size yourself. It's everything else you need to know to win some code golf challenges!

int size
Numbers require 1 nibble (half byte) to initiate number mode and 1 nibble per octal digit of the number in question. So 7 is 1 byte and 8 is 1.5 bytes. Strings are 1 nibble to initiate string mode and then 1 byte per character of the string. So "hi" is 2.5 bytes. Space and newline each require only 1 nibble. Empty string is 2 bytes.

str size

chr size

coercion behavior

implicit args

multiple values

extra values (100, etc)

infinity

arbitrary precision ish

more examples/library

contributing

Bugs, suggestions, and code reviews are appreciated (I'm new to Haskell), email me at nibbles at golfscript com. For suggestions please be familiar with the philosophy.
