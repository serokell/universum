## Description

Version:

## ✓ Checklist for your Pull Request

- [ ] I updated the version number in `universum.cabal`.
- [ ] I updated the [changelog](../tree/master/CHANGES.md) and moved everything
      under the "Unreleased" section to a new section for this release version.
- [ ] If any definitions (functions, type classes, instances, etc) were added,
      I added [`@since` haddock annotations](https://haskell-haddock.readthedocs.io/en/latest/markup.html#since).

## ✓ After merging this Pull Request

- [ ] I created a new entry in the [releases](https://github.com/serokell/universum/releases) page,
      with a summary of all user-facing changes.
    *  I made sure a tag was created using the format `vX.Y.Z`
