# Tutorial: Minutiae

# Not written yet!!

In this final tutorial you will learn obscure features and how to calculate binary nibble code size yourself. It's everything else you need to know to win some code golf challenges!

int size
Numbers require 1 nibble (half byte) to initiate number mode and 1 nibble per octal digit of the number in question. So 7 is 1 byte and 8 is 1.5 bytes. Strings are 1 nibble to initiate string mode and then 1 byte per character of the string. So "hi" is 2.5 bytes. Space and newline each require only 1 nibble. Empty string is 2 bytes.

str size

chr size

coercion behavior

implicit args

extra values (100, etc)

infinity

arbitrary precision ish

implicit args

default int values for stdin empty (100, 1000)

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

But note that if all lines only had 1 or less ints then we would treat it is a single list and not auto map (because that could have just been accomplished using `$`.

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
