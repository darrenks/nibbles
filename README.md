## Welcome to Nibbles (alpha).

Nibbles is a minimal codegolf language. It is also functional, prefix based, statically typed, and lazy. Each instruction is half a byte.

It is in Alpha, meaning things will be drastically changed. There is also minimal documentation (but there is a full [quick reference](https://nibbles.golf/quickref.html)).

If you post answers please designate them as Nibbles Alpha so as not to be confusing later when 1.0 is released.

I am using 8.10 of GHC (but it isn't doing anything fancy), along with the split library (cabal install --lib split).

Compile main.hs for command line use (it will compile nibbles code to out.hs which you then need to compile/run).

## Basics:
Each op is one nibble (half a byte). You code in regular ascii (where each op is 1 character) and can automatically compile to bytes (since most competitions measure bytes).

Syntax is `Expr :: op Expr Expr ... Expr`. The number of args depends on the arity of the op. This is also called prefix notation. So for example ```+ 1 2``` gives ```3```. See [quick reference](https://nibbles.golf/quickref.html) for the full list of ops and examples. In this example `+` takes 2 arguments and the numbers each take 0 arguments.

Functions are implicitly created for ops that require functions (i.e. map). Arguments to functions are referenced using DeBruijn indicies. `$` = 1st, `@` = 2nd, `\2`-`\f` for 3rd-16th. So for example `%,3 +1$` maps through the list `[1,2,3]` and adds 1 to the argument which is the element of the list.

There are essentially 2 types (ints and lists). Strings are actually a list of chars. And chars are just an int, but occasionally have different behaviors (for instance how they are displayed).

`~` Can be used to save space specifying the most common integer for operations. For example `+4~` -> `5` and `*4~` -> `8` (1 is most commonly added to things and 2 is most commonly multiplied by things, see the quickref for what the auto values are for each function). This may not be shorter in literate form, but numbers are actually multiple nibbles (see below).

Input is not supported yet as well as many other things...

Here is a 24 byte (47 nibble) fizzbuzz program (spaces used to show true size):
```
%, 100;:?%$ 3 ""   "Fizz"?%$ 5 ""   "Buzz"?,$$@
```
And how it works.
```
% # map
 ,100 # list of 1 to 100 - note that 100 is actually 4 nibbles, 1 for number mode, then 3 digits of octal.
 ; # a let statement, 1st arg is value, 2nd is expr with value assigned to 1st debruijn index
  : # list concat of the potential fizz and buzz
   ? %$3 "" "Fizz" # if/else statement, note the $ is the 1st debruijn index which is the arg from the map
   ? %$5 "" "Buzz" # note that strings are 1 nibble + 2 * length, except the empty string which is 3.
  ? ,$ $ @ # another if else, @ is the 2nd debruijn index which is the arg from the map now.
```
The final result is a list of strings, which by default are implicitly unlined and printed.

There are lots of pain points in this solution still.

Programs are written is "literate form" (.nbl) files. You can convert them to bytes (.nbb) files by using the `-c` option (and you can convert back to literate form with `-e`). In the binary form instruction ops are heavily overloaded by type, allowing for more than 16 possible instructions. Each character in literate form corresponds to half a byte, with the exception of numbers and strings. Numbers require 1 nibble (half byte) to initiate number mode and 1 nibble per octal digit of the number in question. So 7 is 1 byte and 8 is 1.5 bytes. Strings are 1 nibble to initiate string mode and then 1 byte per character of the string. So "hi" is 2.5 bytes. Space and newline each require only 1 nibble.

Bugs, suggestions, and code reviews are appreciated (I'm new to Haskell), email me at nibbles at golfscript com. For suggestions please be familiar with the philosophy.

## Philosophy:
Codegolf is a game and therefore a codegolf language should be fun (and possibly teach you something). There are tons of great ideas in modern codegolf langauges like Jelly, Husk, Stax, etc. But these languages are very complex with 400+ builtins each. This complexity is bad because it makes golfing in them more about learning hundreds of arbitrary functions than finding novel solutions to the problem. This complexity is the result of pursuing short programs at any cost with a 1 byte instruction set. However even with half byte instructions, more complexity would still allow for shorter problems but complexity would at least have diminishing returns in this case. My goal is to create a simple language that is intuitive and easy to learn, compacting into half bytes is just a simple strategy for shortness that is possible because of this.

It is also a non-goal to be good at everything. For instance there are no regexes, floating point, file reading, rationals, primes, etc. If the problem requires them, then this isn't the language to use. You can't be good at everything without being complicated. Purely algorithmic problems are what it seeks to be good at.

* Regex - because regexs are actually complicated and a language in their own. I have great respect for regexes and problems that can be solved elegantly by them, try perl or todo if you want to use them. Nibbles is more about algorithms than string processing.
* Float - because floats are actually quite arbitrary and problems that use them aren't codegolfy. Why the amount of precision of doubles, floats? The silicone that computes them is actually much more complicated than integers.
* Primes - What could get more algorithmic than these? Yet providing built in lists of primes, primality testing, or factoring would rob you the joy of computing it yourself. Besides this is complicated and not something a normal language has built in. Nibbles is general purpose, not tailored to a specific class of algorithms.

