This is a list of ideas of things that are useless and could be remapped to something else (allowing for multi character builtins).

Only tracking things which take 2 nibbles to achieve.

available extensions
constant + constant (*) too
constant in loop
~ auto valued int (2 in *) (could mean infinity)
`0,1
`unset
length of drop/take? not if they weren't as long as the int
if of constant
value at of a range (although depending on OOB behavior...)
communitiave/associative
opcode 15, mismatched 2d+ lists
ops 8 str int (or str scalar) (l/r just), replace or split/join char or count add int as a char?

desired features
pair (only needed in fns)
min/max
not
or
and
more vectorized ops (- / %)
more special folds
something to convert to truthiness
2** (or 10** if auto 2) (or bitshift)
empty str, empty list of various types
range0
iterate / unfold / until
chunk/while
splitWhen
last/init (can use reverse or foldr for this now)
takeWhile (instead of first of split?)
things that return pair of two things, i.e head, tail ?
group
transpose
array split, not removing empties
maybe version of things that throw?
==, list comparison < >, etc
functions
10
bitwise ^ & |
string + int coercing the otherway (old idea was to use opcode of *int str)
	if could reverse operands of + could make it +str int for good literal form
abs/sig
ljust/etc
uniq, setwise op [by]

-- "too complicated"
sqrt
list of strs shorthand (use 32 value of str?)
magic/hash
permutations/combinations
prefixes/suffixes
base conversion
popcount