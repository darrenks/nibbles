# Tutorial: Minutiae

In this final tutorial you will learn obscure features and how to calculate binary nibble code size yourself. You do not need to know anything here to write Nibbles code, but it will be useful to win some code golf challenges!

## Binary size

All ops correspond to their literate size. `+` is 1 nibble (half a byte). `=\` is 2 nibbles.

**Integers** require 1 nibble plus their size in octal. So, `6` is 1 byte but `8` is 1.5 bytes. `10` and `7` are special cases - their input sizes are swapped since 10 is very common. A negative number takes an extra nibble.

The reason for this encoding is because in binary form only 1 hex value is reserved for starting a number. Then we need to use 1 bit per digit to say when to stop. Since integers are fairly common that is why `~` was added as another way to create integers cheaply.

**Strings** require 1 nibble plus 2 nibbles per character. So "hi" is 2.5 bytes. 

There are a few exceptions:

*	Space and newline each require only 1 nibble.
*	Empty string is 3 nibbles.
*	Binary characters (ascii values <32 or >=127) require 4 nibbles per character (this seems bad, but is necessary to allow for the 1 nibble space/newline which should be more common).

**Chars** aren't that common so require 2 nibbles to initiate and 2 nibbles to encode their value. But since they don't need to use any bits to terminate, they have more special values. The following chars require only 3 nibbles.

*	\n
*	space
*	.
*	,
*	/
*	\`
*	a
*	@
*	A
*	0

Binary chars will require 5 nibbles.

## Coercion Behavior

Some ops require the values to be a certain type. For example the result of `foldr1`. Rather than say your program is invalid if it doesn't match, it coerces the value to the desired type. TODO: write all the rules (in general it is intuitive). See coerceTo in polylib.hs.

Other ops (like `:`) will take in two values and need to coerce them to the same type. First that type is decided and then both are coerced using the same rules as above. TODO: write the rules (but again it is intuitive, one that isn't obvious is char,int which becomes int). See coerce2 in polylib.hs.

## Implicit Args

If you use an operation that expects more values than you provide then Nibbles will choose a value for you to complete the program (so this can only be used for the last operations in a program). It will first choose any unused identifiers (starting from the smallest DeBruijn index). For example:

	p.,;3+$
$Output
	[4,5,6]

Because `$` had been used already, it chose `@` which was the value from the let `;` of 3.

If there are no unused identifiers it will simply choose as many `$` as are needed.

	p.,3+
$Output
	[2,4,6]

Because it is just the same as `p.,3+$$`

`$` will always be something because it corresponds the first integer from stdin at the beginning of your program. However input identifiers are set for optional use and so will not count as unused.

Some 2 nibble list ops require part of their binary representation after the first arg, so they won't be compatible with implicit args, but don't worry too much about this as you will get a useful error message if you do happen to do so.

## Special Values when stdin is Empty

If stdin is empty, it likely means your program is supposed to produce some preset output. In this case constants like 100 are very common and so a few of these become available. Specifically `$` will be `100` and `;$` will be `1000`.

Example (w/ empty input):

	+ *2$ ;$
$Output
	1200

Also `@` becomes the list of printable ascii characters in a more useful order.

	p@
$Output
	" abcdefghijklmnopqrstuvwxyz.,!?_\nABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-+:;\"'~`@#$%^&*()[]{}<>\\/=|"

## Implicit Ops

If your program produces multiple values instead of 1, Nibbles will insert implicit ops. Usually this is conversion to string and then concatenation. For example:

	5 3
$Output
	53

But if the first value is a non-string list then it will attempt to do a foldl1 over the list.

	,5 +@$
$Output
	15

If you don't use the accumulator (`@`) then it will instead assume you wanted to do a map.

	,3+$$
$Output
	2
	4
	6

You could even combine this with implicit args to just

	,3+
$Output
	2
	4
	6

Note that this didn't default to a fold (via `,3+@$`) because these args are set to optional use.

And if you don't even use the element identifier (`$`) then it will assume you wanted to use string concatenation.

	,3 8
$Output
	1
	2
	3
	8

These rules also apply if the first value is an integer, except that it does a "range from 1 to n" to generate a list first.

	3+
$Output
	2
	4
	6

And for a fold `5+@$` which can be done shorter by:

	5+@
$Output
	15

There's a lot more possibilities here, but it isn't obvious what the most common ops at the start of Nibble's programs will be, so we can wait and see what's used in practice the most and then add those.

## Auto Map

If your program uses input but not the entire raw input (`;_` or `;;$`) then your program will auto map.

Suppose the input to your program is

	12 888
	34
	56
	78	

Then outputs for these programs will be:

	p +1$
$Output "12 888\n34\n56\n78"
	13
	889
	35
	57
	79

Because all ints are auto mapped over.

	p @
$Output "12 888\n34\n56\n78"
	"12 888"
	"34"
	"56"
	"78"

Because all lines are auto mapped over.

	p _
$Output "12 888\n34\n56\n78"
	[12,888]
	[34]
	[56]
	[78]
$HiddenOutput "1 2 3"
	[1,2,3]
$HiddenOutput "1\n2\n3"
	[1,2,3]
$HiddenOutput ""
	[]
$HiddenOutput "1"
	[1]
$HiddenOutput "1\na\n2 3"
	[1]
	[2,3]

But note that if all lines only had 1 or less ints then we would treat it is a single list and not auto map (because that could have just been accomplished using `:$`.

	p ;$
$Output "12 888\n34\n56\n78"
	888
	56
	1000

The 1000 resulted because that is the default value of `;$` if there is no value present. It attempted to auto map on pairs of numbers and 78 had no corresponding pair.

	p ;@
$Output "12 888\n34\n56\n78"
	"34"
	"78"
$HiddenOutput "a"
	""

Because it automapped on pairs of lines. Note that if there had been an odd number of lines the last would have been `""`

	p ;_
$HiddenOutput "12 888\n34\n56\n78"
	"12 888\n34\n56\n78"

If auto mapping occurs then the outer list default separator becomes `" "` instead of `"\n"` and the inner list default separator becomes `""` intead of `" "`. This is because if automapping, each output already uses the newline, it's unlikely you'd also want to separate list elements by a newline.

E.g. if your input is

	1 2
	3 4

then

	\_
$Output "1 2\n3 4"
	2 1
	4 3

But if the input was just

	1 2

then 

	\_
$Output "1 2"
	2
	1




## Data

Because string and ints are optimized for typical values, they aren't good at storing data. Ints use 1/4 of their bits to terminate or not, strings 1/8. And this is reasonable because most ints are small, and most strings don't need chars >= 128. However sometimes we just need to store data efficiently!

To do that use `~` after your program and proceed it with a number. This number will consume the rest of your program, but it is stored optimally in the binary format (each nibble now makes up part of a base 16 number). To use this number the first DeBruijn index (`$`) now has the data value instead of the first int of input.

This program is only 14 nibbles instead of 17 encoded in octal.

	$ ~239234902394023 
$HiddenOutput
	239234902394023

You can easily convert it to lists of a desired radix with `to base` (``@`).
 
Some ops use data by default (``D` and `#`). If used, they also prevent the data value from overwriting the first int input value. And after the end of the current root expression data is assumed to start rather than needing ~.

Data can be handy for recreating large strings.

	`D -41 905456382897869687253
$Output
	Hello, world!

Which is 2 nibbles shorter than just using quotes. Not as impressive as compression algorithms tuned to the english language, but this is a more timeless and general technique!

The negative in the radix means to use the list of printable characters rather than straight up base conversion. These values can be found by doing

	ghci header.hs
	> 1 + (maximum $ catMaybes $ map (flip elemIndex printables) $ sToA "Hello, world!")

or if you prefer nibbles:

	`/!@"Hello, world!"?]
$Output
	41

Which tells us the maximum index in the printable chars is 41 for that string. So then we could find the magic number with:

	`@ -41 "Hello, world!"
$Output
	905456382897869687253

FYI data will always be shorter if you are getting it from the auto value of a fn, but for use with `$` it will only be shorter than normal numbers for numbers >= 32768 but it is equally as short for 0 and numbers >= 64 so it could be handy if you use that number more than once.

## Infinity

There is no concept of infinity yet, but it would be nice if there was. TODO create it. For now some things use the value of 2^128 in place of infinity.

## Quine newline suppression.

If your program is a quine (in the binary version). Then don't add on the (usually helpful, but here disastrous) trailing newline to your output...

## Beyond the $QuickRef

TODO I've delete mapaccum for now, so this needs to be updated, but the principles apply.

I haven't built a complete reference of all the built-ins. Most of the useful information is in the $QuickRef. But if you'd like more information, all the ops are defined in [ops.hs](https://github.com/darrenks/nibbles/blob/main/Ops.hs).

Let's see how we could use this to figure out how to use `ma` (`mapAccumL`) (without needing to look at the example).

The arg types are defined as:

	[list, anyT, fn2 (\[l, x]->[x,elemT l])]

This means the first arg is a list, the second is any type, the third is a function that returns 2 values (denoted from fn2). The lambda expression isn't that function's type as you might expect. Instead it is a function which returns what the arg types of this function will be based on the previous args to `ma`. Here it says your function will receive 2 args, the first being the same type as the `anyT` and the second the same type as the element type of the `list`.

Next we have the generated Haskell code:

	"\\l i f->swap $ mapAccumL f i l"

Which should be straightforward if you know Haskell.

Finally we have the return type of this expression which is:

	\[_,x,ft2] -> [vList1$last$ret ft2,x]

This a function from the arg types to `ma` to the return type of `ma`. In this case it is a tuple of:

1.	A list of a tuple1 (which is just a regular value) of the second return type of the function
2.	The type of the initial value.

So basically it is the same type as Haskell's `mapAccumL` with some things switched around. Yes Haskell's type system expresses this much cleaner...

Often there are additional test cases provided in `Ops.hs` which could provide insight into the edge cases of ops.

Good luck.

## Contributing

$Feedback
