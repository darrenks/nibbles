# Tutorial: Tour

In this tutorial you will learn how to use some of the more complicated built ins that aren't immediately obvious from the $QuickRef.

Note that the $QuickRef should print nicely in 3 pages in Chrome/Brave, if it doesn't, [here is a pdf](NibblesQuickRef.pdf). Also FYI you may sort by any column.

There aren't many example here [yet?], but there are many in [Ops.hs](https://github.com/darrenks/nibbles/blob/main/Ops.hs).

## Filter (not&#8728;)
Filter (like many ops that want a function that returns a boolean) have no use for a tuple return, so `~` is taken to mean `not` composed with the function after `~`.

## Zip With
See the zipop table at the bottom of the $QuickRef for all supported ops. `~` can be used to do a true zip with a custom fn.

Note that the 2nd arg doesn't have to be a list, if a scalar it will cycle itself.

Currently most of these ops will not support lists of tuples, but it's high on the todo list to support them.

## Foldr / Scanl
If the second arg is a constant then it is treated as an initial value in the fold, otherwise the first (or last in the case of foldr) element of the list is the initial value.

If `~` precedes the function it means it will be describing an initial value and that value is a tuple.

If the list you are folding on is of a tuple, then `~` just means set initial value (and you would need another `~` to make that initial value a tuple). This is a bit annoying to have two conventions depending on the type of list, so that may change in the future.

### Special Folds / Scans
See the foldop table at the bottom of the $QuickRef for all supported ops. Note that if using `>` or `<` it will take an additional expression and do a max/min by that function.

Currently most of these ops will no support lists of tuples or lists of lists but it's high on the todo list to support them.

## Division
It is safe, in that if you divide by 0 you will get a signed infinity, except that infinity = 2^128...

Modding by 0 will always return 0.

## Drop / Take
Note that `` ` turns them into ops that also return a list of the elements not selected (like split at in Haskell).

If `~` is used for the first arg then take a 3rd arg and drop/take while that fn is truthy. It is compatible with `` `.

Negatives wrap once.

## Subscript
This is 1 indexed and wrapped. If you do not want wrapping (and instead want the default value for the element type if out of bounds), then precede the index with `~`.

## Index
Finds an element index (1 based) or returns 0. If you use `~` instead of an element, then take a fn and find the first index that makes that fn truthy.

## Justify
An obsure op for 1 nibble, but there aren't that many str int ops needed.

-  negative number makes it ljust instead of rjust
-  ~ before the 3rd arg means to center
-  lists for the 3rd arg vectorize (additionally taking the max of length of any element into consideration)

## Char Class
Returns a truthy value if a char satisfies that character class (see end of $QuickRef for a table of char classes). Note that syntax is a little different than you'd expect since we need to know the type of the argument before we parse the char class. E.g. you need to do `\'z'a` instead of `\a 'z'` to see if `'z'` is in the char class `a`.

Also note that a list of the character is returned if true or empty list if false as a return value rather than something like 1 or 0 (this seemed potentially more useful).

## If / Else
-  If you do a length of a list for the condition there is a special hack here to check if the list is null rather than compute the length (which isn't lazy)
-  True clause is a fn that is passed the condition as an argument (this is purely because it seems likely you'll use it).
-	True clause can construct a tuple.
-	False clause `~` will mean default value of the type of the true clause.
-	False clause expects multiple arguments if a tuple was given in true clause.

## Find Indices
If 2nd arg is a fn then find all element indices that make that fn truthy. If 2nd arg is a constant then find all indices of that value.

## Transpose
-  Can work on 1d lists.
-  Can work on list of tuples (returns a tuple of lists). 

## Subsequences
-  0 means all lengths
-  negative number means allow repeats

## List Setwise Ops
-  2nd arg `~` means to do a `uniq` on both args before the operation. `BT&"aasdf" ~"aa"` -> `"a"`
-  2nd arg const means to do the operation normally. `BT&"aasdf" "aa"` -> `"aa"`
-  2nd arg fn means to take a 3rd arg and do the operation by mapping by the 2nd arg first. `BT&"asdf" /$12 "aa"` -> `"ad"`

## Split By
This is a quirky op intended to do things like gsub (but as sophisticated as with regexes). It splits a list by consecutive truthy fn returns. But rather than just splitting, it returns a tuple for each match containing the parts that consecutively matched and didn't. This is so that you could reconstruct the original list but with some modification to the true and/or false parts.

Note that false matches precede the truth matches, so in the case it starts with a truth, then there will be an empty list in that first tuple.

## Base Conversion

For an explanation of ``D` see [Minutiae:Data](tutorial_minutiae.html#data). Note that this is a 3 nibble op, but that the lit form is only 2 because we won't need to pay the price of a nibble to specify that the arg is an int (the op knows it will be a hard coded integer constant).

## Iteration
Iterate while uniq is an enhancement on Haskell's `iterate`. Why would you want to keep iterating once you've seen a repeat (besides not paying the computational cost of checking that)? Since the language is purely functional you are guaranteed to follow an already known cycle. In practice this is useful for skipping having to specify a termination condition. But incase you do want a repeating iteration, use `~` before the function.

Append until null handles an awkward case of iteration, when you wish to iterate, appending to a sequence and being able to reference that entire sequence. There are some enhancements to make this as lazy as possible, but note that it is innately inefficient due to appending to a list repeatedly and Haskell lists can't do this efficiently. In the future I'd like to possibly change this behavior or use a different data structure.

## Hashing
Being able to deterministically map any value to a random integer in a range is frequently useful in code golf. Nibbles makes creating "magic functions" extremely easy and compact. In addition to hashing it also automatically adds salt. It uses the data (see [Minutiae:Data](tutorial_minutiae.html#data)) for salt by converting it to base 256 then adding it to the end of the value as a string. (If you do not want salt then use `~`). You do not need to actually provide data, it will default to empty.

Finding the correct salt is typically done with brute force and Nibbles attempts to make this easy by providing two functions to do it for you. Both take the set of inputs you expect to see, followed by the range you would like answers to be in (0...n). The next arg is the max value you would allow your salt to be (or `~` if you want to let it search forever).

`fs` finds salt that makes those inputs to exact outputs specified in the last arg.

`fsb` finds salt by truthy values from the last arg as a function of the inputs.

I have an idea for designing a hash function that would allow `fs` to not require brute force and be able to find extremely improbable mappings, but it is its own side project and hopefully to come in the future.

FYI this could be a 1 char lit op because the second arg is a hard coded integer, but I like ``#` for now.

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

Recursive functions always need a base case to terminate (actually that's not true for lazy evaluation, but you will probably not be using your functions like this), and the base case is easy to deduce the type of. So recursive functions are implemented as a function that returns 3 things.

1.	the condition for when to use the recursive case
1.	the base case
1. the recursive case

The recursive case doesn't technically have to recurse, but it does have its fixed point added to the context so that you can recurse with `@` (if your function takes 1 argument).

Construct and call a recursive fn with ``;`. Note that you will need to do ```;` if the arg you are passing in is `$` `@` or `_` because this would cause a collision in the binary form for things like `;;$`.

Check out the example in the $QuickRef

FYI Recursion is automatically memoized, you can write a naive fibonacci function and it will run in linear time instead of exponential!
