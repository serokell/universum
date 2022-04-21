Unreleased
=====

* [#176](https://github.com/serokell/universum/issues/176):
  Deprecate `note`.

* [#206](https://github.com/serokell/universum/issues/206):
  Remove `listToMaybe`.
  _Migration guide:_ use `safeHead` directly with functions from
  `Universum.Container` instead.

1.7.3
=====

* [#236](https://github.com/serokell/universum/issues/236):
  Add `updateMVar'` and `updateTVar'`.
* [#244](https://github.com/serokell/universum/issues/244)
  Add `ToPairs` instances for `[(k, v)]` and `NonEmpty (k, v)`.

* [#238](https://github.com/serokell/universum/pull/238):
  Add `fromList`.

1.7.2 (rev1)
=====

* Permit `text-1.2.5.0`.

1.7.2
=====

* Permit `text-1.2.4.1`.
* [#233](https://github.com/serokell/universum/issues/223):
  Add `someNE`.

1.7.1
=====

* [#230](https://github.com/serokell/universum/issues/230):
  Add `hoistMaybe` and `hoistEither` functions
  similar to [relude](https://hackage.haskell.org/package/relude-0.7.0.0/docs/Relude-Monad-Trans.html#v:hoistMaybe)

1.7.0
=====

* [#221](https://github.com/serokell/universum/issues/221):
  Add safe versions of `minimum`, `maximum`, `minimumBy`, `maximumBy`, `foldr1`, `foldl1` functions for `NonEmpty` list.
  Old their versions from `Container` typeclass now return `Maybe` and have
  `safe` prefix in name (e.g. `safeMinimum`).
  Add unsafe versions of those functions to `Unsafe` module.
* [#185](https://github.com/serokell/universum/issues/185):
  Enable more warnings, fix all warnings.

1.6.1
=====

* [#219](https://github.com/serokell/universum/issues/219):
  Bump upper bound on `text`.

1.6.0
=====

* [#207](https://github.com/serokell/universum/pull/207):
  Remove various monad transformer combinators, `flipfoldl'`, and `<<$>>`
  from the list of changes suggested in `.hlint.yaml`.
* [#214](https://github.com/serokell/universum/issues/214):
  Update supported GHC versions (replace 7.10.3 with 8.6.5).

* [#212](https://github.com/serokell/universum/issues/212)
  Added rewrite rule for `toString . toText` case.
  This may change semantics in some corner cases
  (because `toString . toText` is not strictly the identity function).

* [#215](https://github.com/serokell/universum/pull/215):
  Fix docstrings in `Universum.Lifted.File` to mention correct module when
  referencing related functions.

1.5.0
=====

* Make `error`'s stacktrace exclude site of the `error` function itself.

* [#200](https://github.com/serokell/universum/pull/200):
  Implemented a lifted version of `withFile` and added `hClose` to
  `Universum.Lifted.File` as discussed previously in
  [#186](https://github.com/serokell/universum/issues/186).

* [#204](https://github.com/serokell/universum/issues/186):
  Make `trace` non-polymorphic over text argument, add `traceIdWith` and `traceShowIdWith`.

* [#197](https://github.com/serokell/universum/pull/197) `hPutStr`, `hPutStrLn`
  and `hPrint` added to `Universum.Print`. The interface for the backing
  typeclass `Universum.Print.Print` changed. It was also moved to the internal
  module `Universum.Print.Internal` and should be considered unstable.

  _Migration guide:_ The interface for the `Print` class should be considered
  internal and may be subject to sudden change. If you **must** implement your
  own instances, then import `Universum.Print.Internal` (be aware that there are
  name clashes in the functions from `Universum.Print` and
  `Universum.Print.Internal`)

* [#201](https://github.com/serokell/universum/pull/201) Generalized the type of
  `Universum.Lifted.Env.die`. Should not break existing code, apart from,
  perhaps, type inference.

1.4.0
=====

* [#167](https://github.com/serokell/universum/issues/164):
  `identity` has been removed.

  _Migration guide:_ use `Universum.id` instead.
* [#177](https://github.com/serokell/universum/issues/177):
  The `mask_` reexport from `safe-exceptions` has been removed.

  _Migration guide:_ use `Control.Exception.Safe.mask_` from `safe-exceptions`
  instead.
* [#178](https://github.com/serokell/universum/issues/178):
  `getArgs` has been removed.

  _Migration guide:_ use `liftIO` directly with `System.Environment.getArgs`
  from base.
* [#179](https://github.com/serokell/universum/issues/179):
  `getContents` and `interact` have been removed.

  _Migration guide:_ use `liftIO` directly with `Data.Text.Lazy.IO.getContents`
  and `Data.Text.Lazy.IO.interact`, both from the `text` package.
* [#180](https://github.com/serokell/universum/issues/180):
  The `Lifted.ST` module has been removed.

  _Migration guide:_ use `liftIO` directly with functions from
  `Control.Monad.ST` instead.
* [#181](https://github.com/serokell/universum/issues/181):
  `list` has been removed.

1.3.0
=====

* [#167](https://github.com/serokell/universum/issues/164):
  `identity` has been deprecated.

  _Migration guide:_ use `Universum.id` instead.
* [#170](https://github.com/serokell/universum/pull/170):
  Remove `ElementConstraint` from the `Container` class.

  _Migration guide:_ remove `ElementConstraint` from every instance and every type signature.
* [#174](https://github.com/serokell/universum/issues/174)
  The `type-operators` dependency has been removed.
* [#177](https://github.com/serokell/universum/issues/177):
  The `mask_` reexport from `safe-exceptions` has been deprecated.

  _Migration_guide:_ use `Control.Exception.Safe.mask_` from `safe-exceptions`
  instead.
* [#178](https://github.com/serokell/universum/issues/178):
  `getArgs` has been deprecated. To be removed in a future version.

  _Migration guide:_ use `liftIO` directly with `System.Environment.getArgs`
  from `base`.
* [#179](https://github.com/serokell/universum/issues/179):
  `getContents` and `interact` have been deprecated.

  _Migration guide:_ use `liftIO` directly with `Data.Text.Lazy.IO.getContents`
  and `Data.Text.Lazy.IO.interact`, both from the `text` package.
* [#180](https://github.com/serokell/universum/issues/180):
  The `Lifted.ST` module has been deprecated. To be removed in a future
  version.

  _Migration guide:_ use `liftIO` directly with functions from
  `Control.Monad.ST` instead.
* [#181](https://github.com/serokell/universum/issues/181):
  `list` has been deprecated. To be removed in a future version.

1.2.0
=====

* [#159](https://github.com/serokell/universum/issues/159) **Breaking change**:
  Remove `text-format` dependency.

  _Migration guide:_ import `Buildable` type class either from `text-format` or `formatting` or `fmt` library. There is no direct replacement for `pretty` and `prettyL` in popular libraries. You can define `prettyL = Data.Text.Lazy.Builder.toLazyText . build` and `pretty = Data.Text.Lazy.toStrict` . prettyL`.
* [#164](https://github.com/serokell/universum/issues/164):
  Don't reexport `log :: Floating a => a -> a`.

1.1.1
=====

* [#148](https://github.com/serokell/universum/issues/148):
  Add `CODEOWNERS` and contributing guide.
* [#135](https://github.com/serokell/universum/issues/135):
  Add documentation regarding internal module structure.
* [#113](https://github.com/serokell/universum/issues/113):
  Annotate `at` function from `Unsafe` module and `ordNub`
  function from `Nub` module with `liquidhaskell`.
* [#73](https://github.com/serokell/universum/issues/73):
  Add more examples to docs and fix warnings where possible.
* Move reexport of `NonEmpty` to `Universum.List` module.

1.1.0
=====

* [#144](https://github.com/serokell/universum/issues/144):
  Add `Exc` pattern synonym.
* [#60](https://github.com/serokell/universum/issues/60):
  Reexport `Natural` type from `Numeric.Natura` module.
* [#118](https://github.com/serokell/universum/issues/118):
  Reexport `Type` from `Data.Kind` module.
* [#130](https://github.com/serokell/universum/issues/130):
  Merge `ToList` and `Container` type classes into single type class `Container`.
* [#15](https://github.com/serokell/universum/issues/15):
  Add `?:` function to `Universum.Monad.Maybe`.
* [#128](https://github.com/serokell/universum/issues/128):
  Add `Unsafe` module with unsafe functions to works with lists and `Maybe`.
* [#129](https://github.com/serokell/universum/issues/129):
  Reexport `id`.
* [#136](https://github.com/serokell/universum/issues/136):
  Change `foldl'` type back, add `flipfoldl'` instead.

1.0.4.1
=====

* [#127](https://github.com/serokell/universum/issues/127):
  Fix `doctest` for `text-1.2.3`.

1.0.4
=====

* [#53](https://github.com/serokell/universum/issues/53):
  Add `doctest` to `universum`. Also imporove and fix documentation.
* [#117](https://github.com/serokell/universum/issues/117):
  Drop the support of `GHC-8.0.1`.
* [#104](https://github.com/serokell/universum/issues/104):
  Reexport `hashWithSalt` from `Data.Hashable`.
* [#95](https://github.com/serokell/universum/issues/95):
  Reexport `Compose` from `Data.Functor.Compose`.
* [#124](https://github.com/serokell/universum/issues/124):
  Export methods of class `Exception`.

1.0.3
=====

* [#114](https://github.com/serokell/universum/issues/114):
  Reexport more functions from `safe-exceptions`.

1.0.2
=====

* [#91](https://github.com/serokell/universum/issues/91):
  Change argument order of `foldl'`.
* [#97](https://github.com/serokell/universum/issues/97):
  Add `ToPairs` type class with the ability to have list of pairs.

1.0.1
=====

* [#100](https://github.com/serokell/universum/issues/100):
  Add `bug` function = `impureThrow`.

1.0.0
=====

* [#90](https://github.com/serokell/universum/issues/90):
  Improve project structure.
* [#89](https://github.com/serokell/universum/issues/89):
  Add export of `Universum.Nub` module to `Universum`.
* Add `listToMaybe` to `Universum.Monad.Reexport`.
* [#81](https://github.com/serokell/universum/issues/81):
  Make `putText` and `putLText` to be versions of `putStr`.
  Add `putTextLn` and `putLTextLn` -- versions of `putStrLn`.
* [#5](https://github.com/serokell/universum/issues/5):
  Add safe versions of `head`, `tail`, `init`, `last` functions for `NonEmpty` list.
  Old `head` (which returns `Maybe`) is renamed to `safeHead`.
  Reexports from `safe` are removed.
* Remove `unsnoc` (this function is very slow and shouldn't be used).
* [#88](https://github.com/serokell/universum/issues/88):
  Add `HasCallStack =>` to `error` and `undefined` functions.
* [#58](https://github.com/serokell/universum/issues/58):
  Make `Element` type family be associated type family.
  Remove `{-# OVERLAPPABLE #-}` instance for `ToList` and `Container`. Add default instances for basic types.
  Remove `WrappedList` `newtype` because it's not needed anymore.
  Remove `NontrivialContainer` constraint alias.
* [#56](https://github.com/serokell/universum/issues/56):
  Make `elem` and `notElem` faster for `Set` and `HashSet` by introducing `ElementConstraint` associated type family.
* Remove `Unsafe` module. Though, see issue [#128](https://github.com/serokell/universum/issues/128)
  for disuccion regarding possible return of this module.

0.9.1
=====

* Change `base` version to be `< 5`.

0.9.0
=====

* [#79](https://github.com/serokell/universum/issues/79):
  Import '(<>)' from Semigroup, not Monoid.
* Improve travis configartion.
* [#80](https://github.com/serokell/universum/issues/80):
  Rename `Container` to `ToList`, `NontrivialContainer` to `Container`.
  Keep `NontrivialContainer` as type alias.
* Rename `Containers` module to `Container.Class`.
* Move all container-related reexports from `Universum` to `Container.Reexport`.
* Add default implementation of `null` function.
* Add `WrappedList` newtype with instance of `Container`.
* Improve compile time error messages for disallowed instances.

0.8.0
=====

* [#83](https://github.com/serokell/universum/issues/83):
  Change the order of types in `show` and `print` functions.
* Move string related reexports and functions to `Conv` module.
* Rename `Conv` module to `String`.
* Move `print` function to `Print` module.
* [#77](https://github.com/serokell/universum/issues/77):
  Add `modify'` function to export list.

0.7.1.1
=======

* [#69](https://github.com/serokell/universum/issues/69):
  Document `SuperComposition` operator `(...)`.

0.7.1
=====

* [#68](https://github.com/serokell/universum/issues/68):
  Separate all 'nub' functions to `Nub` module, add `sortNub` and `unstableNub` there.
* [#54](https://github.com/serokell/universum/issues/54):
  Reorganize .cabal.
* [#21](https://github.com/serokell/universum/issues/21):
  Add benchmarks.
* [#65](https://github.com/serokell/universum/issues/65):
  Use `TypeNats` instead of `TypeLits` when possible.

0.7.0
=====

* [#47](https://github.com/serokell/universum/issues/47):
  Reexport `put` and `get` for `MonadState`.
* [#48](https://github.com/serokell/universum/issues/48):
  Export boxed `Vector` type.
* [#49](https://github.com/serokell/universum/issues/49):
  Export `IdentityT` and `runIdentityT`.
* [#51](https://github.com/serokell/universum/issues/51):
  Add `fromRight` and `fromLeft` that behave like `fromMaybe` but for `Either`.
* [#52](https://github.com/serokell/universum/issues/52):
  Add `maybeToMonoid :: Monoid m => Maybe m -> m`.
* Remove `Symbol`-related types for sure.
* Return back seems to be useful function `guardM` removed in `v0.3`.
* Add `notElem` for `NonTrivialContainer`.

0.6.1
=====

* Fixed version number bug (it had 4 numbers).

0.6.0.0
=======

* [#62](https://github.com/serokell/universum/issues/62):
  Export exceptions-related functions from 'safe-exceptions'.

0.5.1
=====

* Fix an infinite loop in `decodeUtf8` from `Text` to `ByteString.Lazy`.

0.5
===

* Export `MonadTrans` typeclass.
* Remove `Symbol`-related exports from `GHC.TypeLits`.
* Remove `SrcLoc` and `Location` reexports from `GHC.ExecutionStack`.
* Add `With` type operator.
* Add `hashNub`.
* Export strict `StateT` instead of lazy.

0.4.3
=====

* Assign associativity and priority to (...), export typeclass itself.

0.4.2
=====

* [#25](https://github.com/serokell/universum/issues/25):
  Add vararg functions composition operator (...).
* Rewrite `concatMapM` & `concatForM` so that they allow traversed
  and returned-by-function container types differ.

0.4.1
=====

* Reexport `sortWith` from `GHC.Exts`.

0.4
===

* Add _haddock_ documentation with 100% coverage.
* Rewrite README tutorial.
* [#37](https://github.com/serokell/universum/issues/37):
  Add generalized version of `readEither`.
* [#38](https://github.com/serokell/universum/issues/38):
  Add `evaluateNF`, `evaluateNF_`, `evaluateWHNF`, `evaluateWHNF_`.
* [#39](https://github.com/serokell/universum/issues/39):
  Add lifted versions of `IORef` functions.
* Remove `foreach`
* Reexport `(&&&)` from `Control.Arrow`.
* Add lifted version of `readTVarIO`.
* `interact` and `getContents` work with _Lazy Text_.
* Reexport `MaybeT`, `maybeToExceptT`, `exceptToMaybeT`.

0.3
===

* [#28](https://github.com/serokell/universum/issues/28):
  Remove `putByteString` and `putLByteString`.
* [#29](https://github.com/serokell/universum/issues/29):
  Remove `panic`, `FatalError` and `notImplemented`.
  Rename `NotImplemented` into `Undefined`.
* [#32](https://github.com/serokell/universum/issues/32):
  Remove `orAlt`, `orEmpty`, `liftAA2`, `eitherA`, `purer`, `<<*>>`,
  `traceIO`, `guardM`, `hush`, `tryIO`, `liftM'`, `liftM2'`,
  `applyN`, `guardedA`,
  Bifunctor instances for tuples of length higher than 2.
  Generalize `concatMapM`, add `concatForM` and operator versions.
* [#35](https://github.com/serokell/universum/issues/35):
  Generalize `andM`, `orM`, `allM`, `anyM` over container type.

0.2.2
=====

* [#33](https://github.com/serokell/universum/issues/33):
  Add `($)` and `Each` type operators.

0.2.1
=====

* [#24](https://github.com/serokell/universum/issues/26):
  Add `whenNothing`, `whenNothing_`, `whenNothingM`, `whenNothingM_`,
  `whenLeft`, `whenLeftM`, `whenRight`, `whenRightM`,
  `whenNotNull`, `whenNotNullM`.
* [#26](https://github.com/serokell/universum/issues/24):
   Add `usingReader`, `usingReaderT`,
       `usingState`, `usingStateT`,
       `executingState`, `executingStateT`,
       `evaluatingState`, `evaluatingStateT`.
* Remove `maybeToEither`.

0.2
===

* Add `one` (similar to `singleton`).
* Expose `Symbol` and `Nat` types from `GHC.TypeLits` by default.
* Export `genericLength` and other generic list return functions.
* Rename `msg` to `fatalErrorMessage`.
* Export `ExceptT`
* Export `ReaderT`, and `StateT` constructors.
* Export `NonEmpty` type and constructor for Base 4.9 only.
* Export `Data.Semigroup` type and functions for Base 4.9 only.
* Export `String`.

0.1.13
======

* Add lenses from `microlens`.
* Add `(<&>)`.
* Reexport `(&)` from `Data.Function` if it's present there instead
  of always defining our own (this is actually done by reexporting it
  from `Lens.Micro` which does the right thing).
* Fix a space leak in `whenJust`.

0.1.12
======

* Use custom classes instead of `Foldable`. Thanks to this, `length` and similar functions can't anymore be used on tuples or `Maybe`, but can be used on e.g. `Text`, `ByteString` and `IntSet`.

* Add `allM`, `anyM,` `andM`, `orM`.

* Reexport `fail` and `MonadFail`.

0.1.11
======

* Expose `putByteString` and `putLByteString` monomorphic versions of `putStrLn` functions
* Switch exported `(<>)` to be from `Data.Monoid` instead of Semigroup.
* Export `Hashable`

0.1.10
======

* Generalize most `IO` functions to `MonadIO`
* Make `die` available for older versions of base

0.1.9
=====

* Make `sum` and `product` strict

0.1.8
=====

* ``foreach`` for applicative traversals.
* ``hush`` function for error handling.
* ``tryIO`` function for error handling.
* ``pass`` function for noop applicative branches.
* Mask ``Handler`` typeclass export.
* Mask ``yield`` function export.

0.1.7
=====

* Export monadic ``(>>)`` operator by default.
* Add ``traceId`` and ``traceShowId`` functions.
* Export``reader`` and ``state``  functions by default.
* Export lifted ``throwIO`` and ``throwTo`` functions.

0.1.6
=====

* Add uncatchable panic exception throwing using Text message.
* Remove ``printf``
* Remove ``string-conv`` dependency so Stack build works without ``extra-deps``.
* Bring ``Callstack`` machinery in for GHC 8.x.
* Remove ``throw`` and ``assert`` from ``Control.Exception`` exports.
* Remove ``unsafeShiftL`` and ``unsafeShiftR`` from ``Data.Bits`` exports.
* Reexport ``throw`` as ``unsafeThrow`` via Unsafe module.
* Hides all Show class functions. Only the Class itself is exported. Forbids custom instances that are not GHC derived.
* Export`` encodeUtf8`` and ``decodeUtf8`` functions by default.
* Adds ``unsnoc`` function.

0.1.5
=====

* Initial release.
