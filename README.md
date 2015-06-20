# What is this?

`msi-kb-backlit` is a simple tool mainly designed for Linux, where the Steel Series Engine doesn’t work. It’s used to
change colors and modes of MSI keyboards such as the one on MSI GS60. However, there’s no reason the tool doesn’t work
on Windows as well!

# How to install?

# ArchLinux

I maintain the package. It’s in the [AUR](https://aur4.archlinux.org/packages/msi-kb-backlit). You should either
get the [tarball](https://aur4.archlinux.org/cgit/aur.git/snapshot/msi-kb-backlit.tar.gz), or use `yaourt`:

    yaourt -Sy msi-kb-backlit

# Others

You’ll need [cabal-install](https://wiki.haskell.org/Cabal-Install) and a recent version of
[GHC](https://www.haskell.org/ghc/).

    cabal update
    cabal install msi-kb-backlit

# Usage

The can get the usage of the tool by running it with no argument.

    msi-kb-backlit
