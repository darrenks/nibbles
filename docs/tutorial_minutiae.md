# Tutorial: Minutiae

In this final tutorial you will learn obscure features and how to calculate binary nibble code size yourself. You do not need to know anything here to write Nibbles code, but it will be useful to win some code golf challenges!

## Binary size

All ops correspond to their literate size. `+` is 1 nibble (half a byte). `st` is 2 nibbles.

**Integers** require 1 nibble plus their size in octal. So, `7` is 1 byte but `8` is 1.5 bytes. The reason for this is because in binary form only 1 hex value is reserved for starting a number. Then we need to use 1 bit per digit to say when to stop. Since integers are fairly common that is why `~` was added as another way to create integers cheaply.

If the number is a power of 10 then its size is 1 less nibble (this trick is enabled by remapping the encoding for a leading 0 in a number).

**Strings** require 1 nibble plus 2 nibbles per character. So "hi" is 2.5 bytes. 

There are a few exceptions:

*	Space and newline each require only 1 nibble.
*	Empty string is 3 nibbles.
*	Binary characters (ascii values <32 or >=127) require 4 nibbles per character (this seems bad, but is necessary to allow for the 1 nibble space/newline which should be more common).

**Chars** aren't that common so require 2 nibbles to initiate and 2 nibbles to encode their value. But since they don't need to use any bits to terminate, they have more special values. The following chars require only 3 nibbles.

*	\n
*	space
*	,
*	.
*	-
*	0
*	a

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

## Special Values when stdin is Empty

If stdin is empty, it likely means your program is supposed to produce some preset output. In this case constants like 100 are very common and so a few of these become available. Specifically `$` will be `100` and `;$` will be `1000`.

Example (w/ empty input):

	+ *2$ ;$
$Output
	1200

## Implicit Ops

If your program produces multiple values instead of 1, Nibbles will insert implicit ops. Usually this is conversion to string and then concatenation. For example:

	5 3
$Output
	53

But if the first value is a non-string list then it will attempt to do a foldr1 over the list.

	,5 +$@
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

Note that this didn't default to a fold (via `,3+$@`) because these args are set to optional use.

And if you don't even use the element identifier (`$`) then it will assume you wanted to use string concatenation.

	,3 8
$Output
	1
	2
	3
	8

Unless that second value is a string, then it will assume you wanted to join on that string.

	,3 ", "
$Output
	1, 2, 3

These rules (except the join one) also apply if the first value is an integer, except that it does a "range from 1 to n" to generate a list first.

	3+
$Output
	2
	4
	6

And for a fold `5+$@` which can be done shorter by:

	5+@
$Output
	15

There's a lot more possibilities here, but it isn't obvious what the most common ops at the start of Nibble's programs will be, so we can wait and see what's used in practice the most and then add those.

## Auto Map

If your program uses input but not the entire raw input (`;_`) then your program will auto map.

Suppose the input to your program is

	12 888
	34
	56
	78	

Then outputs for these programs will be:

	p +$1
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

But note that if all lines only had 1 or less ints then we would treat it is a single list and not auto map (because that could have just been accomplished using `:~$`.

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

## Infinity

There is no concept of infinity yet, but it would be nice if there was. TODO create it.

## Arbitrary precision ish

For most use of integers nibbles doesn't specify if it is an Integer or Int (the former means infinite precision in Haskell), and so it is left up to Haskell to decide. This was done in attempt to make it much more efficient, but in practice Haskell cannot do a very good job deciding with the little information Nibbles gives it (and there could very well be some bugs where one is an Int and another Integer - which would cause it not to compile). It probably would have been better to build a type safe interpreter than trying to create Haskell code (this was also done for efficiency, but is bad because the string manipulation lacks type safety information). The short term plan is to just switch everything to Integer. The long term plan would be to switch it to being an interpeter and doing bounded integer analysis to determine when things can be safely operated on as Ints. That's a lot of work potentially but could have some other cool uses for additional overloading.

## Beyond the $QuickRef

I haven't built a complete reference of all the built-ins. Most of the useful information is in the $QuickRef. But if you'd like more information, all the ops are defined in [ops.hs](https://github.com/darrenks/nibbles/blob/main/ops.hs).

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

Often there are additional test cases provided in `ops.hs` which could provide insight into the edge cases of ops.

Good luck.

## Contributing

$Feedback
