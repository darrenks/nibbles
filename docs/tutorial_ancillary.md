# Tutorial: Ancillary

In this tutorial you will learn some useful features to help you reach par in some code golf problems.

## Laziness
So you might have been wondering how using the input args work. When does it parse the input? What if I use both `@` and `_`? What if I use it multiple times? The answers are all very simple (if you understand laziness). Laziness is the whole reason Nibbles uses Haskell.

When does it parse the input? This is the wrong question to be asking in a purely functional language. But more practically speaking, your program won't get stuck waiting for input if you don't use any input variables. If your program unexpectedly hangs you may have accidentally used an input var, causing it to wait for input.

What if I use both `@` and `_`? They both work independently, sharing the same input.

What if I use it multiple times? It will return the same result each time. In fact everything in nibbles is a pure function.

### Infinite lists and errors
If this is all unfamiliar to you, you could read up on [lazy evaluation](https://en.wikipedia.org/wiki/Lazy_evaluation). But you'll do ok with just the information here, as laziness should be pure win compared to eager evaluation (for code golf purposes). Anything you would do eagerly will still work in equal or better asymptotic time lazily.

Typical examples to show off laziness revolve around not throwing an error if you never use a value. For example `/1 0` throws an error but if we do something like `=1 :2 /1 0` -> `2` (which builds a list of `[2,error]`, then takes the first element), it never uses the value of `/1 0` and so therefore never errors.

And we could also generate the list of numbers from 1 to a googol, then select only the first 5 that are odd as such:

	<5 | ,^10 100 %$2
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

	<1 |      # Get the first 1 elements of the filtered list.
	  >1,$    # Generate the list from 2 to input
	  - 1 %@$ # \elem -> 1 - (input % elem)
$HiddenOutput "3902309423233451"
	436151

That was hard, and there are still some pain points we haven't learned how to get around yet, like having to extract the input number from a list twice (and differently even since there was something added to the context). The key thing for this lesson though is we generated a list up to the original input number which was guaranteed to contain a factor, but we didn't have to pay the computational cost of checking all numbers, nor did we need to explicitly exit the loop.

$EndSolution

### Practical Tricks

The best way to get the first element of a list actually relies on laziness, and that is doing a foldr1 and just returning the element.

	/,^10 100 $
$Output
	1

Of course this trick doesn't require laziness to return the right result, but in that example, it does if you want your program to actually finish. A similar trick can be used to get the last element of a list (by using `@` instead of `$`). But this will not be lazy, nor could any operation on a simple cons style list.	

## Output

So far we've just been printing a single value or string. But if your program returns a list it is printed with newlines between each element. And if it is a list of lists then spaces between those inner elements. Example:

	.,3 .,3 +@$
$Output
	2 3 4
	3 4 5
	4 5 6

Lists of dimension &#8805; 3 first concatenate their inner dimensions to become 2 dimensions. For printing purposes a string is considered a single value (not a list of chars).

If the default behavior isn't what you want, you can fairly easily increase or decrease the dimensions of your list using `+` (concat) or `: expr ~` (singleton).

### Multiple Outputs

You can return multiple things, e.g.

	+2 1
	+4 3
$Output
	37

They will just be printed without any separators. But beware, you may accidentally use some implicit ops besides concatenation (see [Implicit Ops](tutorial_minutiae.html#implicitops) for more info).

## Auto Values

`~` Can be used to save space by specifying the most common integer for operations in a single nibble. They don't allow you to do anything new, but they make it shorter. For example the most common number used in addition is 1 (that's why it often has special forms like `++` in C and `succ` in Ruby/etc.). So for addition `~` is `1` (which would have required two nibbles).

To make this concrete: `+4~` -> `5`

You can probably guess the auto values for each operation, but they are also listed in a column in the full $QuickRef.

## Let Statements

`;` is a let statement and is somewhat special. It takes one argument and returns it, but also saves that argument for use by anything after it. You reference it in the same way you do for function arguments. For example `+ ;2 $` is the same as `+ 2 2`. Note that the scope of this variable is limited to the same scope as its highest level dependency. E.g. if you use a loop variable the let variable can only be used within that loop.

### Exercise

Write a program to print the sum of all numbers in the input or the string "large" if that sum is greater than 100 (without computing the sum twice).

One unexpected thing to warn you about is the true and false clauses of a `?` have the first argument (conditional value) passed to them which could save you an assignment if you wish to use that int.

$Solution

	? - ;+_ 100 "large" $
$HiddenOutput "1 2 3"
	6
$HiddenOutput "50 51"
	large

$EndSolution

## Tuples

When I said `;` is "somewhat special" I was somewhat lying. Anytime something returns multiple values, everything after the first value is automatically splatted onto the context. So `;` really just takes one value, then returns "a tuple" of that value and itself. Tuples really are somewhat special though (not first class), they will never be bound to an identifier.

For example ``/` is a function that means `divmod`. It is just a function which returns two values. You could use it like this:

	"the div is: " `/ 10 3 "\n"
	"the mod is: " $
$HiddenOutput
	the div is: 3
	the mod is: 1

Note if you wanted to use the value of the mod before the div, you can't do that with divmod, so there is also a function ``%` which computes moddiv. In general functions that return multiple things will have multiple versions due to this drawback.

### Creating Tuples

In functions you can return a tuple instead of a regular value using `~`. For example:

	p.,5 ~$ *$$
$Output
	[(1,1),(2,4),(3,9),(4,16),(5,25)]

Outside of functions it would be pointless to create your own tuple, it would immediately be deconstructed, and so that opcode has been rebound to something else, making it invalid to even try.

You may have multiple `~` in a row to create three tuples or higher.

## Vectorization

Check out the full $QuickRef. Notice that the `+` and `*` ops actually take a `vec` instead of `num`. All this means is that the `vec` arg can actually be a list of any dimension and that the operation will be applied to all elements.

For example `+5 ,5` -> `[6,7,8,9,10]`. This is cool, but not as useful as in other golf languages because it prevents overloading by type, so it is only provided for these very common operations.

## Extensions

You've seen an example of extensions already, ``/` (divmod). If you look at the binary representation of divmod it is `9 d`. This would normally correspond to `< num , num`, but it is pointless to do a `take` after using `range`. At best you could have wanted a range on the min of the two nums, but that could be done equally as short with `,[num num` So we remap `9 d` to divmod and give it a new name so that you don't need to remember that `< ,` is divmod.

In general you do not actually have to think about extensions, it is all abstracted away. But it is useful for understanding naming conventions and why there are the number of built-ins that there are. Also you may accidentally use an extension (e.g. if did try to take on a range). Nibbles will give you a warning if you do this.

Note that there is no limit to the number of extensions that can be created, but in order to keep the language simple I have decided to limit extensions to 2 nibbles (in most cases). There are probably hundreds of possible 3 nibble extensions, but finding them and avoiding extension collisions would be extremely unmanageable, besides a primary goal of Nibbles is to be simple (few operations) anyway.

If you find possible two nibble extensions, please let me know!

### Commutative Extensions

Alright these things are going to be annoying, but their efficiency cannot be passed up. There's no reason to make `+1 2` do the same thing as `+2 1` so we can remap one of the orders so long as there is a way to statically say which way the args are ordered. We say that the order is for `+` when the left operand is larger or equal to the right operand (and `]` which means `max` otherwise). But by larger we don't mean the value, but the length of the nibbles binary code (or lexicographically as a tie breaker). So in general if you want to use `+` put the longer code first. This should always be possible but it might not always be easy to put the small operand first since if both sides use the values of let statements, the lets will always need to be in the first operand. I think it is possible to factor that code out and will try to do this in the future.


### reqfn and const Extensions

Some ops have a function arg where it usually wouldn't make sense to ignore the arg. Filter for example. We can detect that and instead use that opcode for something else. In filter's (`|`) case, if the snd arg ignores the element arg then it will perform a "zip with" (`!`) instead. (There is a separate literate name for this so that you don't accidentally do the wrong thing, but they have the same binary form). This isn't quite a perfect abstraction though because when using `!` you need to be aware that all DeBruijn indices are 1 higher than normal when entering the second argument. You can see these extensions in the $QuickRef if the arg type is reqfn or const.
