debug features to add:
	gather rest of program
	show context values
	show arg types
	verbose literate mode (and/or print tree) (with arg names, types, desc, indentation)

For alpha release:
-tutorial (and auto test tutorial examples)
-make sure handle type pair everywhere in Polylib and elsewhere (i.e. take first before operating)
	-but should it default to using first or both?
-io
-maybe

minor things:
-coerce after foldr
-10 as leading 0 in int rep

multiple values makes special behavior (i.e. if int constant, range map?)?
------------------------------------------
compile with wall on
-------------------------------
auto input types
library reference?

non empty list for many [VT]

fyi instead of coerce in user fn, could skip it in context lookup

for multipage quickref (coerce, etc)
https://stackoverflow.com/questions/1664049/can-i-force-a-page-break-in-html-printing

debug helpers
	see current context
	show type of arg and exit
	see the parse tree

website design

better op not found error message (hard because memoize of arg types is lazy)
aliases so don't have to use ~ in middle of extensions type : "1" ~
helpful error if communitive backwards (use aliases)

either instead of bool/maybe
maybe id, maybe nothing
handle div 0 errors/etc
infinity

index/prev in loop

more extensions
	communitive extensions

auto fill $ for implicit input (or unused args that were expected to be used) (let's wait and see what would be most useful)

auto range1 map if number then multiple outputs? also could auto map if list?

quickref to show return types