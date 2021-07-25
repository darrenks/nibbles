# Install

### Install Haskell
Follow instructions at [https://www.haskell.org/platform/](https://www.haskell.org/platform/)

You will need **GHC 8.10.4** and **cabal 3.4.0.0** (or greater).

### Install some libs

	> cabal install --lib dlist split pureMD5

### Install Nibbles
Download the [latest stable version](https://nibbles.golf/nibbles-latest.tgz), unpack it then compile it.

	> ghc -O -Wno-tabs *.hs

This creates the `nibbles` binary. Do whatever you do to put binaries in your path.

### Other version of Haskell/cabal

If you already have Haskell installed, you may check compatibility with `ruby test/testall.rb`. Theoretically it should be possible to use older versions but you will run into issues with libs and modules being different, feel free to let me know how you were able to get it running on older versions if you do so.
