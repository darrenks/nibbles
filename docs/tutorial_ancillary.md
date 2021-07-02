# Tutorial: Ancillary

In this tutorial you will learn some useful features to help you reach par in some code golf problems.

## Laziness
So you might have been wondering how using the input args work. When does it parse the input? What if I use both `@` and `_`? What if I use it multiple times? The answers are all very simple (if you understand laziness). Laziness is the whole reason Nibbles uses Haskell.

When does it parse the input? This is the wrong question to be asking in a purely functional language. But more practically speaking, your program won't get stuck waiting for input if you don't use any input variables. If your program unexpectedly hangs you may have accidentally used an input var, causing it to wait for input.

What if I use both `@` and `_`? They both work independently, sharing the same input.

What if I use it multiple times? It will return the same result each time. In fact everything in nibbles is a pure function.

### Infinite lists and errors
If this is all unfamiliar to you, you could read up on [lazy evaluation](https://en.wikipedia.org/wiki/Lazy_evaluation). But you'll do ok with just the information here, as laziness should be pure win compared to eager evaluation (for codegolf purposes). Anything you would do eagerly will still work in equal or better asymptotic time lazily.

Typical examples to show off laziness revolve around not throwing an error if you never use a value. For example `/1 0` throws an error but if we do something like `=\: /1 0 2 1` -> `2` (which builds the list of `[error,2]`, reverses it then takes the first element), it never uses the value of `/1 0` and so therefore never errors.

And we could also generate the list of numbers from 1 to a googol, then select only the first 5 that are odd as such:

	<5 & ,^10 100 %$2
$HiddenOutput
	1
	3
	5
	7
	9

Without it using all of the time the universe has to offer. This is useful when we don't know how many elements we will need at later stages of computations (typically languages have a separate concept of streams to support this, but that is superflous in a lazy language).

### Full Laziness

Another useful thing about Haskell's laziness (full laziness). Is that expressions are never evaluated multiple times, even inside a loop. For example:

	+,100000000

Computes the sum from 1 to 100,000,000, and takes 1.16 seconds on my computer not including compile time.

	+ .,1000 +,100000000

Which computes the sum of that sum in a loop 1,000 times takes only 1.33 seconds. If you had done this in a strict language it would have taken 1,000 times longer. Yes, some optimizing compilers in languages like C might have been smart enough to automatically move that computation out of the loop, but in general they cannot because their type system doesn't understand side effects.

This is critical for the whole model of Nibbles' syntax (since there would be no easy way to move an expensive computation from inside a loop to out).

### <span style="color: red">Challenge</span> Exercise
Write a program that finds one factor of a composite number from stdin, let's say 3902309423233451. You may not hard code constants besides numbers <= 2.

Hint: If you'd like to negate a "bool" don't forget about the custom truthiness rules for ints.

$Solution

	<1 &          # Get the first 1 elements of the filtered list.
	  >1,=@ 1     # Generate the list from 2 to input
	  - 1 %=_ 1$ # \elem -> 1 - (input % elem)
$HiddenOutput "3902309423233451"
	436151

This is the first time we've needed comments, use them with `#`

That was hard, and there are still some pain points we haven't learned how to get around yet, like having to extract the input number from a list twice (and differently even since there was something added to the context). The key thing for this lesson though is we generated a list up to the original input number which was guaranteed to contain a factor, but we didn't have to pay the computational cost of checking all numbers, nor did we need to explicitly exit the loop.

$EndSolution

## Output

So far we've just been printing a single value or string. But if your program returns a list it is printed with newlines between each element. And if it is a list of lists then spaces between those inner elements. Example:

	.,3 .,3 $
$Output
	1 2 3
	1 2 3
	1 2 3

Lists of dimension &#8805; 3 first concatenate their inner dimensions to become 2 dimensions. For printing purposes a string is considered a single value (not a list of chars).

If the default behavior isn't what you want, you can fairly easily increase or decrease the dimensions of your list using `+` (concat) or `:~` (singleton).

### Multiple Outputs

You can return multiple things, e.g.

	+1 2
	+3 4
$Output
	37

They will just be printed without any separators. This behavior is likely to change in the future (todo).

## More Inputs

There will be automatic parses beyond `$`, `@`, `` `2`` at the start of the program, they will be mentioned here when they exist (todo).

## Auto Values

`~` Can be used to save space by specifying the most common integer for operations in a single nibble. They don't allow you to do anything new, but they make it shorter. For example the most common number used in addition is 1 (that's why it often has special forms like `++` in C and `succ` in Ruby/etc.). So for addition `~` is `1` (which would have required two nibbles).

To make this concrete: `+4~` -> `5`

You can probably guess the auto values for each operation, but they are also listed in a column in the full $QuickRef.

## Let Statements

`;` is a let statement and is somewhat special. It takes one argument and returns it, but also saves that argument for use by anything after it. You reference it in the same way you do for function arguments. For example `+ ;2 $` is the same as `+ 2 2`. Note that the scope of this variable is limited to the same scope as its highest level dependency. E.g. if you use a loop variable the let variable can only be used within that loop.

### Exercise

Write a program to print the sum of all numbers in the input or the string "large" if that sum is greater than 100 (without computing the sum twice).

$Solution

	? - ;+@ 100 "large" $
$HiddenOutput "1 2 3"
	6
$HiddenOutput "50 51"
	large

$EndSolution

## Tuples

When I said `;` is "somewhat special" I was somewhat lying. Anytime something returns multiple values, everything after the first value is automatically splatted onto the context. So `;` really just takes one value, then returns "a tuple" of that value and itself. Tuples really are somewhat special though (not first class), they will never be bound to an identifier.

For example `/~` is a function that means `divmod`. It is just a function which returns two values. You could use it like this:

	"the div is: " /~ 10 3 "\n"
	"the mod is: " $
$HiddenOutput
	the div is: 3
	the mod is: 1

Note if you wanted to use the value of the mod before the div, you can't do that with divmod, so there is also a function `%~` which computes moddiv. In general functions that return multiple things will have multiple versions due to this drawback.

### Creating Tuples

In functions you can return a tuple instead of a regular value using `~`. For example:

	p.,5 ~$ *$$
$Output
	[(1,1),(2,4),(3,9),(4,16),(5,25)]

Outside of functions it would be pointless to create your own tuple, it would immediately be deconstructed, and so that opcode has been rebound to something else, making it invalid to even try.

## Maybe

todo

## Vectorization

Check out the full $QuickRef. Notice that the `+` and `*` ops actually take a `vec` instead of `num`. All this means is that the `vec` arg can actually be a list of any dimension and that the operation will be applied to all elements.

For example `+5 ,5` -> `[6,7,8,9,10]`. This is cool, but not as useful as in other golf languages because it prevents overloading by type, so it is only provided for these very common operations.

## Extensions

You've seen an example of extensions already, `/~` (divmod). Extensions are just a remapping of the behavior of something that would be useless to something useful. There isn't an auto value for the numerator of div that would be canonical (1 would be if floats were used in Nibbles, but they are not). So we just remap this to do something else. Here divmod is related to div so we keep the literate form inline with the binary. But sometimes this would be confusing, for example, reversing a list twice has been remapped to sort. Rather than make you memorize that `\\` means sort, sort is just named `st` in the literate form.

In general you do not actually have to think about extensions, it is all abstracted away. But it is useful for understanding naming conventions and why there are the number of built-ins that there are. Also you may accidentally use an extension (e.g. if you tried to reverse a list). Nibbles will give you an error if you do this in the literate form.

Note that there is no limit to the number of extensions that can be created, but in order to keep the language simple I have decided to limit extensions to 2 nibbles (for now at least). This is reasonable in that a 3 nibble op has an implied probability of use as 1/2<sup>12</sup> which would mean that very few programs would use it and I don't wish to convolute the language with things that are of little use.

If you find possible two nibble extensions, please let me know!

## Creating Your Own Functions
It is actually somewhat rare to need to create your own functions in codegolf. But none-the-less there are times that it could definitely be useful to apply the same logic in unrelated parts of the program.

This can be done using the `;;` extension. `;;` takes two arguments, treating the second one as a function and passing it the first. But it also adds that function to the context for later use.

Example:

	;;3 *$$
$Output
	9
and now we can use that square function with `$`. E.g. `$4` gives `16`.

Note that we can even square strings now. E.g. `$"5"` gives `25`.

It may seem odd that we couldn't define a function without using it. But this is codegolf, why would you want to!

### Exercise

Define your own divmod (and call it)!

Hint: The first argument to `;;` is actually a function of no arguments (which allows you to pass a tuple into your function).

$Solution

	p :                   # pretty print 1st call
		;; ~10 3 ~/$@ %$@  # create fn and call it
		$                  # get the snd result
	p :                   # pretty print 2nd call
		@ 20 5             # call it again
		$                  # get the snd result
$Output
	[3,1][4,0]

Notice that the second use of your function didn't require you to use `~`. It knew it took two arguments.

$EndSolution

### Recursive Functions

Unfortunately recursive functions (without type specifications) are difficult to implement because when we recurse we don't yet know the return type. Luckily there is a hacky way to get around this, that actually makes the code even shorter!

Recursive functions always need a base case to terminate, and the base case is easy to deduce the type of. So recursive functions are implemented as a function that returns 3 things.

1.	the condition for when to use the base case
1.	the base case
1. the recursive case.

The recursive case doesn't technically have to recurse, but it does have its fixed point added to the context so that you can recurse with `@` (if your function takes 1 argument).

Create a recursive funciton with `;~`.

Check out the example in the $QuickRef