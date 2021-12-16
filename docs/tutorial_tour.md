# Tutorial: Tour

In this tutorial you will learn how to use some of the more complicate built ins that aren't immediately obvious from the $QuickRef

## Creating Your Own Functions
It is actually somewhat rare to need to create your own functions in code golf. But none-the-less there are times that it could definitely be useful to apply the same logic in unrelated parts of the program.

This can be done using the `;~` extension. `;~` takes two arguments, treating the second one as a function and passing it the first. But it also adds that function to the context for later use.

Example:

	;~3 *$$
$Output
	9
and now we can use that square function with `$`. E.g. `$4` gives `16`.

Note that we can even square strings now. E.g. `$"5"` gives `25`.

It may seem odd that we couldn't define a function without using it. But this is code golf, why would you want to!

### Exercise

Define your own divmod (and call it)!

Hint: The first argument to `;~` is actually a function of no arguments (which allows you to pass a tuple into your function).

$Solution

	p :                   # pretty print 1st call
	   ;~ ~10 3 ~/$@ %$@  # create fn and call it
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
1. the recursive case

The recursive case doesn't technically have to recurse, but it does have its fixed point added to the context so that you can recurse with `@` (if your function takes 1 argument).

Create a recursive funciton with `;~`.

Check out the example in the $QuickRef

## Append

## Exponentiation

## Filter (not)

## Zip with

## Foldr / Scanl

### Special Folds / Scans

## Drop / Take

## Subscript

## Index

## Justify

## Char Class

## If / Else

## Find indices

## Transpose

## Subsequences

## List Setwise Ops

## Bit Ops

## Split By

## Split List

## Base conversion

## Iteration

## Hashing
