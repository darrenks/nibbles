# Try Nibbles

## Playground

[Attempt This Online](https://ato.pxeger.com/run?1=m708LzMpKSe1eMGCpaUlaboW65U8UnNy8nUUyvOLclIUlSCiUEmYIgA)

## Building from source

### Install GHC
Nibbles compiles to Haskell and therefore requires a suitable compiler.
We strongly recommend using [GHCup](https://www.haskell.org/ghcup/) to obtain a copy of GHC, the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/).

> [!NOTE]  
> At the time of writing, the Nibbles project is fully compatible with [all released versions](https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC%20Status#all-released-ghc-versions) from **8.0.2** through **9.12.2**.

### Build Nibbles
Download the tarball file of the [latest release](nibbles-latest.tgz) and extract the archive.
You can also download a tarball file from a [previous release](downloads.html) or clone the [project repository](https://github.com/darrenks/nibbles) on GitHub.

Compile the project to generate the `nibbles` executable in the current working directory.

```bash
$ cd nibbles
$ ghc -O -package ghc nibbles.hs
```

> [!NOTE]  
> If the compilation aborts prematurely with errors, please refer to the section on [installing packages](try_it.md#install-packages).

> [!IMPORTANT]  
> Future source files containing Nibbles code should be compiled and then run from the directory containing the `nibbles` executable, except when installing the packages in the next section.

### Install packages

Release tarballs (.tgz) contain bundled package dependencies. However, if you prefer to install them manually, you can do so as follows:

```bash
$ rm -r Data
$ cabal install --lib bytestring containers dlist filepath memoize mtl murmur-hash process split template-haskell
```

> [!WARNING]  
> Starting with GHC version **9.8.1**, you should omit "memoize" from the above installation command and download the package tarball from [Hackage](https://hackage.haskell.org/package/memoize), the Haskell package repository. You can also clone the [package repository](https://github.com/tov/memoize) on GitHub or download the same tarball from the [Releases](https://github.com/tov/memoize/releases) page.
>
> Run the following command from the package root directory:
> 
> ```bash
> $ sed -i 's/()/BndrVis/' src/Data/Function/Memoize/TH.hs
> ```
>
> Alternatively, depending on how you obtained the package source code, you can manually replace the `()` on [this line](https://github.com/tov/memoize/blob/dc745b32068ff2411228ccbb70e5b86ee193aad8/src/Data/Function/Memoize/TH.hs#L159) with `BndrVis`.

### Run tests (optional)

If you want to check the compatibility of the Nibbles project with your current Haskell compiler, you can run the following command:

```bash
$ ruby test/testall.rb
```

---

> [!TIP]
> If you encounter any issues with compilation and/or installation, please [report](https://github.com/darrenks/nibbles/issues/new) them via GitHub.
