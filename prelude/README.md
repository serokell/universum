Universum-prelude
=================

For medium-sized projects, it is tedious to write `import Universum` in every single module, while switching to a project-custom prelude may be not yet desired.

This package re-exports `Universum` module as `Prelude`, effectively allowing implicit import of universum.
When adding a dependncy on `universum-prelude` to your project, you will need to replace `base` dependency with `base-noprelude` to avoid conflicts.

Additionally, `Universum.Unsafe` is re-exported as `Unsafe` module.
