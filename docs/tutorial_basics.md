# Tutorial: Basics

This basics tutorial will teach you how to write and run Nibbles code. You will have a feel for the language and be able to solve real code golf problems, but some things will still be awkward and imperfect. It assumes basic familiarity with and ability to run unix commands and that you know how to code. Knowledge of Haskell is useful but not required.

---

In Nibbles, each instruction only requires 4 bits (also know as half a byte - aka a nibble). But it's nice to read and write in ascii (literate form), so we write code in ascii and compile it, packing 2 instructions per byte (binary form). In the binary form, ops are heavily overloaded by type, allowing for more than 16 possible instructions.

Typically the literate code you write will correspond 1 ascii character to 1 nibble, so you can basically divide your code length in half. There are some exceptions to this (numbers, strings, etc). But don't worry about this for now.

Let's see this in action.

### Install Haskell
Follow instructions at [https://www.haskell.org/platform/](https://www.haskell.org/platform/)

### Install some libs
	> cabal install --lib dlist
	> cabal install --lib split

### Install Nibbles
Download the [latest stable version](https://nibbles.golf/nibbles-latest.tgz), unpack it then compile it.

	> ghc -O -Wno-tabs nibbles.hs

Do whatever you do to put binaries in your path

### Create a simple program
Here's a simple Nibbles program in literate form:

	+1 2

Save that to `intro.nbl` (nbl is the file extension for literate Nibbles programs).

### Run it

	> nibbles intro.nbl
<!-- -->

	size = 5 nibbles
	3

In addition to running the program, it also output the compactified size of your program to stderr.

You may have also noticed that a file named `out.hs` appeared. For now, Nibbles works by compiling to Haskell.

###  Compress it to binary form

	> nibbles -c intro.nbl

A file appears named `intro.nbb` (nbb is the file extension for binary Nibbles programs) which has a size of 3 bytes. That compactification was not as good as the promised divide by 2... but it would have been 2.5 bytes if we were measuring that way (and small numbers are a bad example). FYI you can automatically expand `.nbb` files back to literate form using `-e`.

## Syntax
Programs are written in Polish (prefix notation). Instead of writing `1+2` we write `+1 2`. The advantage of this is that you do not ever need parenthesis. `(1+2)*3` would be written as `*+1 2 3`. `1+(2*3)` would be written as `+1*2 3`. This may seem strange for math operators but it is actually quite familiar for other functions. For example C uses prefix notation for function calls. i.e. `add(1, 2)`. But unlike C we don't need commas or parenthesis since all functions have a fixed arity.

### Exercise
Write a program that computes `(1+2)*(3-4)`
<details>
<summary>Solution</summary>


	*+1 2-3 4

You may be distraught about those spaces, but they don't affect binary size.
</details>

## Other Types
You've seen **integers**, other data types are chars and lists. **Chars** behave like integers in most cases but are displayed differently (the ops table linked below makes this more precise). They can be created with single quotes, i.e. `' '` to create a space char.

**Lists** can be created with the `:` (append) operator. However unlike Haskell, `:` coerces its operands, turns non lists into singleton lists and then concatenates them. For example `::1 2 3` produces `[1,2,3]`. Note that this bracket output is just for output display purposes and not valid Nibbles code.

**Strings** are actually just a list of chars, and can be created using double quotes, for example `"Hello\n"` does what you would expect. Note that escapes are valid for strings and chars in the same style that Haskell uses.

Here is [a table of the basic Nibbles ops](https://nibbles.golf/simpleref.html). There is a more complete quick reference, but for now it would have some confusing things.

### Exercise
Use the ops table to write a program which outputs your name as a square, i.e

	Darren
	Darren
	Darren
	Darren
	Darren
	Darren

<details>
<summary>Solution</summary>

	^6:"Darren" '\n'

That newline could have been included in the string but I'm just showing off how not to code golf.
</details>

## Functions
There were a couple things in that table we haven't seen yet, functions and args. For example `.` (map's) second argument is a function and the first is the list it will map over.

Functions do not require any syntax, some ops take a function argument and automatically treat that operand as a function. Any expression can be treated as a function, for example if `+1 2` is to be treated as a function with 1 argument, then it is simply a function which ignores its argument and returns 3!

Args are referenced by number with `$` `@` `` `3 `` `` `4 `` `` `5 `` `` `6 `` `` `7 `` `` `8 `` `` `9 `` `` `a `` `` `b `` `` `c `` `` `d `` `` `e `` `` `f ``. If you are nesting functions then these count upwards in scope. For example in an inner function of 1 argument, `$` would be its argument as usual, but `@` would be the first argument of the outer function. This is also known as DeBruijn indices.

See `map`, `foldr`, etc in the ops table for examples.

### Exercise
Compute the product of all even numbers less than 50 (the answer is `10409396852733332453861621760000`). And yes this number is > 2<sup>64</sup> but Nibbles uses arbitrary precision when appropriate, so don't worry about that.

**Hint1**: the range operator (`,`) is 1 indexed, which may seem bad, but, as it turns out in code golf, is much more commonly used than lists starting from 0. So `range` and all other list operators in Nibbles are 1 indexed.

**Hint2**: the filter operator (`&`) uses the truthiness of the function result. Integers are considered truthy if they are > 0, lists are turthy if non empty. Chars are truthy if they are non whitespace.

<details>
<summary>Solution</summary>

	/&,49%+1$2*$@
Finally we are seeing nice looking programs, can you do better? We will learn more ways to shorten this.
</details>

## Input
All programs actually starts off with args available for use.

- `$` is the entire stdin as a string
- `@` is the entire stdin parsed into a list of ints (with non ints removed)
- `` `3 `` is the the entire stdin split into lines (a list of strings).

For example

	> echo "+@" > intro.nbl
	> echo 1 2 3 | nibbles intro.nbl
<!-- -->

	6

There are more, but for now this should suffice.

It can be hard to keep track of what DeBruijn index corresponds to what, so you can always use `ct` to see **c**ontext **t**ypes.

### Exercise
Reverse every line in the input.

<details>
<summary>Solution</summary>

	.`3\$
  
Were you expecting it to be harder than that?
</details>
