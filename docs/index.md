# Nibbles Home

$Intro

Currently there are 85 ops which is far fewer than other competitive competitive golf languages which are typically around 400 ops. There are some special rules explained in [minutiae](tutorial_minutiae.html), but overall the core language is simple too. Most of the work on the implementation is for static type analysis, error handling, and managing the automatic conversion between the literate and binary form. Things that you don't have to think about!

In my experience on [golf.shinh.org](http://golf.shinh.org) nibbles is shorter than GS2 and Jelly about [75%](stats.html#nibblesversusotherlanguagesongolfshinhorg) of the time. It is also much easier to write thanks to optional identifiers in the literate form.

### Example: FizzBuzz

This program appears as 28 characters here, but each takes only half a byte in the binary form (except for the digits and string text). It is trivially encoded as **18 bytes**. That's pretty decent considering 8 of those bytes are for the text FizzBuzz.

   $?,:^-~%$3"Fizz"^-~%$5"Buzz"
$HiddenOutput
   1
   2
   Fizz
   4
   Buzz
   Fizz
   7
   8
   Fizz
   Buzz
   11
   Fizz
   13
   14
   FizzBuzz
   16
   17
   Fizz
   19
   Buzz
   Fizz
   22
   23
   Fizz
   Buzz
   26
   Fizz
   28
   29
   FizzBuzz
   31
   32
   Fizz
   34
   Buzz
   Fizz
   37
   38
   Fizz
   Buzz
   41
   Fizz
   43
   44
   FizzBuzz
   46
   47
   Fizz
   49
   Buzz
   Fizz
   52
   53
   Fizz
   Buzz
   56
   Fizz
   58
   59
   FizzBuzz
   61
   62
   Fizz
   64
   Buzz
   Fizz
   67
   68
   Fizz
   Buzz
   71
   Fizz
   73
   74
   FizzBuzz
   76
   77
   Fizz
   79
   Buzz
   Fizz
   82
   83
   Fizz
   Buzz
   86
   Fizz
   88
   89
   FizzBuzz
   91
   92
   Fizz
   94
   Buzz
   Fizz
   97
   98
   Fizz
   Buzz

### Resources

The [tutorials](tutorial_basics.html) can teach you the language.

Also check out the [Why page](why.html) if you're interested in the ideas behind Nibbles.

### Where to Golf?

Good places to use Nibbles for competing online are:

-  [Anarchy Golf](http://golf.shinh.org/) (my old favorite and still good)
-  [Code Golf Stack Exchange](https://codegolf.stackexchange.com) (most active)

### Feedback?

$Feedback

## News
- **March 4, 2022**: Add [Stats](stats.html) page which analyzes Nibbles ops use and win rate.

- **Jan 24, 2022**: Check out [Nibbles Commenter by tails](http://www.tailsteam.com/cgi-bin/nbbdag/index.pl) for more verbose decompiles of nibbles solutions on anarchy golf. Especially nice for solutions to older problems with changes that weren't backwards compatible.

- **Dec 21, 2021**: Nibbles is in **Beta**. Happy solstice and golfing! The language should be mostly stable except for bug fixes and documentation for a while.

- **June 29, 2021**: Nibbles is in **Alpha**! If you post code please designate it as Nibbles Alpha so as not to be confusing later when it becomes broken by language changes.

