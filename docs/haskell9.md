# Building with Haskell 9

Yewzir has figured out how to build Nibbles with Haskell 9. I am putting the instructions here in order to not complicate the installation process. It is easier to just use 8.7 if you can choose your Haskell version, which is what is was developed on. Ideally I would have used a .cabal file which I assume would allow it to configure which package versions to use so that it can work with later versions more easily. However the old approach will continue to work for 8.7 since it includes copies of the correct versions of required libs.

Yewzir has tested this method on **8.0.2** through **9.12.2** and confirms it working.

## Instructions

### Follow regular build instructions with these differences:

Get required packages:

    > rm -r Data
    > cabal install --lib bytestring containers dlist filepath mtl murmur-hash process split template-haskell

"Memoize" is omitted from the above installation command. Download the package tarball from [Hackage](https://hackage.haskell.org/package/memoize), the Haskell package repository. You can also clone the [package repository](https://github.com/tov/memoize) on GitHub or download the same tarball from the [Releases](https://github.com/tov/memoize/releases) page.

Run the following command from the package root directory:

    > sed -i 's/()/BndrVis/' src/Data/Function/Memoize/TH.hs

Alternatively, depending on how you obtained the package source code, you can manually replace the `()` on [this line](https://github.com/tov/memoize/blob/dc745b32068ff2411228ccbb70e5b86ee193aad8/src/Data/Function/Memoize/TH.hs#L159) with `BndrVis`.
