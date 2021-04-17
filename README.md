Welcome to Nibbles (alpha).

Nibbles is a minimal codegolf language. It is also functional, prefix based, statically typed, and lazy. Each instruction is half a byte.

It is in Alpha, meaning things will be drastically changed (and there isn't proper documentation yet). If you post answers please designate them as Nibbles Alpha so as not to be confusing later when 1.0 is released.

I am using 8.10 of GHC (but it isn't doing anything fancy), along with the split library (cabal install --lib split).

Compile main.hs for command line use (it will compile nibbles code to out.hs which you then need to compile/run). See the getValue function in compile.hs for a list of functions you can use.

A quick summary:
There are essentially 2 types (ints and lists). Strings are actually a list of chars. And chars are just an int, but occasionally have different behaviors (for instance how they are displayed). (In compile.hs the bool in the int type is whether or not the int is a char - this is done for easy pattern matching).

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

Bugs, suggestions, and code reviews are appreciated (I'm new to Haskell), email me at nibbles at golfscript com. Please read the philosophy first if is about a design choice or new function.

# Philosophy:
Codegolf is a game and therefore a codegolf language should be fun (and possibly teach you something). There are tons of great ideas in modern codegolf langauges like Jelly, Husk, Stax, etc. But these languages are very complex with 400+ builtins each. This complexity is bad because it makes golfing in them more about learning hundreds of arbitrary functions than finding novel solutions to the problem. This complexity is the result of pursuing short programs at any cost with a 1 byte instruction set. However even with half byte instructions more complexity would still allow for shorter problems but complexity would at least have diminishing returns in this case. My goal is to create a simple language that is intuitive and easy to learn, compacting into half bytes is just a simple strategy for shortness that is possible because of this.

It is also a non-goal to be good at everything. For instance there are no regexes, floating point, file reading, rationals, primes, etc. If the problem requires them, then this isn't the language to use. You can't be good at everything without being complicated. Purely algorithmic problems are what it seeks to be good at.

* Regex - because regexs are actually complicated and a language in their own. I have great respect for regexes and problems that can be solved elegantly by them, try perl or todo if you want to use them. Nibbles is more about algorithms than string processing.
* Float - because floats are actually quite arbitrary and problems that use them aren't codegolfy. Why the amount of precision of doubles, floats? The silicone that computes them is actually much more complicated than integers.
* Primes - What could get more algorithmic than these? Yet providing built in lists of primes, primality testing, or factoring would rob you the joy of computing it yourself. Besides this is complicated and not something a normal language has built in. Nibbles is general purpose, not tailored to a specific class of algorithms.

Unfortunately nibbles is less pure than other codegolf languages in that we won't want to write the raw bytes ourselves (although bytes really are quite arbitrary). So I've created a literate form. But I want codegolfing to still feel like regular codegolfing, so I've aimed to have each character in the literate form equal 1 nibble (half a byte). The exception to this is strings and numbers. Which would require you to either convert them into hex/octal or require you to explicitly insert padding characters to denote their true length. In the interest of making it easy, the compiler automatically converts these, insert your own spaces if you want them to display how long they really are. This is why extensions are multiple characters, i.e. `Or` instead of `|`, because behind the scenes this instruction is actually two nibbles.
