# Install

### Install Haskell
You will need **GHC 8.0.2 - 8.10.7**. I recommend using [ghcup](https://www.haskell.org/ghcup/) to get it.

### Install Nibbles
Download the [latest stable version](https://nibbles.golf/nibbles-latest.tgz), unpack it then compile it.

	> cd nibbles
	> ghc -O -package ghc -Wno-tabs *.hs

This creates the `nibbles` binary. Do whatever you do to put binaries in your path.

### Note on libraries

The .tgz file contains the source of the libraries it depends on. If you prefer to instead download them do so as follows.

	> rm -r Data # (removes the predownloaded libraries)
	> cabal install --lib dlist split murmur-hash memoize


### Other versions of Haskell

Other versions of Haskell might work, I just haven't tried them yet. You may run all the nibbles tests to check compatibility with `ruby test/testall.rb`. Please let me know if you have any troubles with 8.0.2 - 8.10.7 or if you are able to get other versions to work and how.
