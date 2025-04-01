Nibbles is a [code golf](https://en.wikipedia.org/wiki/Code_golf) language for mortals (it aims to be good at code golf while remaining relatively simple). It is also purely functional, statically typed, and fully lazy. Syntax is Polish (prefix) notation. Each instruction is half a byte when automatically converted from ascii to the binary form.

## Status

I am no longer actively maintaining Nibbles. However I am happy to accept pull requests for fixes if anyone is motivated. Also, feel free to file github issues to keep a record of known problems. Nibbles has been used enough, that there shouldn't be major issues. My reasons for not continuing to fix bugs myself are:

-   I no longer believe the in the philosophy behind it. (Aiming to be the best compared to other languages incentivizes complexity and overfitting common code golf problems). As it is now, Nibbles is more complex than I would like and doesn't have some features that I like such as vectorization.
-   It is difficult to maintain. Nibbles is written in Haskell and compiles to Haskell. I was not and am not experienced at creating large projects in Haskell, and find it difficult to revisit some of the more complicated parts of the code. I simply do not have the time to dig into it anymore.

I recommend my latest language: [iogii](../iogii) which I do plan on maintaining. That being said, I do not consider Nibbles to be a bad language, I would still be happy to see it in use.

See [golfscript.com/nibbles](http://golfscript.com/nibbles) for tutorials and references.
