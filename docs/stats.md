# Stats

Thanks to [tails](https://twitter.com/saito_ta) (for both creating a better [disassembler](http://www.tailsteam.com/cgi-bin/nbbdag/index.pl) and submitting many solutions). We can have some nice statistics on op usage. This information could be useful for modifying Nibbles or anyone creating future languages.

Also thanks to shinh for hosting a well organized golf server. It would be more difficult to do statistics like this from the stackexchange problems because there isn't a standard way to document input and many problems will use command line args of which their type needs to be known at compile time (one would have to manually curate inputs).

Code to generate data is on [github](https://github.com/darrenks/nibbles/tree/main/stats/shinh).

-  [atoms.txt](atoms.txt) - atom count frequency
-  [ops.txt](ops.txt) - op use frequency based on the types of its arguments
-  [combos.txt](combos.txt) - counts of ops/atoms used together (not just in the first argument)

## Analsysis on March 19, 2023

It is difficult to draw many sound conclusions because even though the data set contains 200 solutions, many are short, and when you break binary ops down by both argument types, the counts aren't that high. They could easily be biased by which types of problems have been solved. None-the-less, a few things to note:

[atoms.txt](atoms.txt):

-  2 is the most common number
-  mostly what you'd expect
-  allLines is used more often than expected

[ops.txt](ops.txt):

-  ` ``@ ` to base is more commonly used than expected
-  `;` save is not used often on strings, but decently often on ints
-  `:` append is not often used on some type combos.
-  implicit fold is only used 2, might be worth removing.
-  This doesn't make it easy to see what's missing, i.e. if some op is 1 char and never used, we don't see it.

[combos.txt](combos.txt):

-  ` ``@ 2 ` is very common
-  also ^2 x
-  map on a range1 common
-  concat on a map common

Action items:

-  move allLines up in DeBruijn indicies
-  add to base 2 op (even if 3 nibble)

Feel free to let me know any other conlcusions you draw!

## TODO stats on implicit ops

## Nibbles versus other languages on golf.shinh.org

As of March 1, 2022:

Comparing to GS2 ([leading language at golf.shinh.org](http://golf.shinh.org/lranking.rb)) and Jelly ([leading language at stackexchange](https://codegolf.meta.stackexchange.com/questions/8798/golfing-class-of-a-language))

For problems where both Nibbles and Jelly were used

-  Nibbles shorter: 106
-  Tie: 25
-  Jelly shorter: 8

For problems where both Nibbles and GS2 were used

-  Nibbles shorter: 149
-  Tie: 37
-  GS2 shorter: 34

One possible bias is that Jelly was created mostly to use command args instead of stdin (although stdin is supported too, but I'm not sure how well) (all input is via stdin on this server).

As of Dec 2022 on
[codegolf.stackexchange.com](https://codegolf.stackexchange.com) Nibbles won 44, tied 9, and lost 23 to Jelly. So it seems it is the #1 language on that site as well! Thanks to Dominic van Essen for the many great Nibbles solutions to give more data here.

Reading too much into this data would a mistake as it could be gamed (for instance if people only submitted solutions to problems their favorite language would win), but at face value it is promising.
