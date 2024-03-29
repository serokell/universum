{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}

{- | Main module that reexports all functionality allowed to use
without importing any other modules. Just add next lines to your
module to replace default 'Prelude' with better one.

@
\{\-\# LANGUAGE NoImplicitPrelude \#\-\}

__import__ "Universum"
@

This documentation section contains description of internal module structure to
help navigate between modules, search for interesting functionalities and
understand where you need to put your new changes.

Functions and types are distributed across multiple modules and grouped by
meaning or __theme__. Name of the module should give you hints regarding what
this module contains. Some /themes/ contain a great amount of both reexported
functions and functions of our own. To make it easier to understand these huge
chunks of functions, all reexported stuff is moved into separate module with
name @Universum.SomeTheme.Reexport@ and our own functions and types are in
@Universum.SomeTheme.SomeName@. For example, see modules
"Universum.Container.Class" and "Universum.Container.Reexport".

Below is a short description of what you can find under different modules:

* __"Universum.Applicative"__: reexports from "Control.Applicative" and some
  general-purpose applicative combinators.
* __"Universum.Base"__: different general types and type classes from @base@
  package ('Int', 'Num', 'Generic', etc.) not exported by other modules.
* __"Universum.Bool"__: 'Bool' data type with different predicates and combinators.
* __"Universum.Container"__: 'Foldable' replacement, types from @containers@
   and @unordered-containers@ and @vector@ packages.
* __"Universum.Debug"__: @trace@-like debugging functions with compile-time
  warnings (so you don't forget to remove them)
* __"Universum.DeepSeq"__: reexports from "Control.DeepSeq" module and
  functions to evaluate expressions to weak-head normal form or normal form.
* __"Universum.Exception"__: reexports "Control.Exception.Safe" from
  @safe-exceptions@ package, 'bug' as better 'error', 'Exc' pattern synonym for
  convenient pattern-matching on exceptions.
* __"Universum.Function"__: almost everything from "Data.Function" module.
* __"Universum.Functor"__: reexports from "Data.Functor", "Data.Bifunctor",
  other useful 'Functor' combinators.
* __"Universum.Lifted"__: lifted to 'MonadIO' functions to work with console,
  files, 'IORef's, 'MVar's, etc.
* __"Universum.List"__: big chunk of "Data.List", 'NonEmpty' type and
  functions for this type ('head', 'tail', 'last', 'init').
* __"Universum.Monad"__: monad transormers, combinators for 'Maybe' and 'Either'.
* __"Universum.Monoid"__: reexports from "Data.Monoid" and "Data.Semigroup".
* __"Universum.Nub"__: better versions of @nub@ function for list.
* __"Universum.Print"__: polymorphic 'putStrLn' function and functions to print 'Text'.
* __"Universum.String"__: reexports from @text@ and @bytestring@ packages with
    conversion functions between different textual types.
* __"Universum.TypeOps"__: convenient and fancy type-level operators.
* __"Universum.Unsafe"__: unsafe functions (produce 'error').
  Not exported by "Universum" module by default.
* __"Universum.VarArg"__: variadic composition operator '...'.
-}

module Universum
       ( -- * Reexports from base and from modules in this repo
         module Universum.Applicative
       , module Universum.Base
       , module Universum.Bool
       , module Universum.Container
       , module Universum.Debug
       , module Universum.DeepSeq
       , module Universum.Exception
       , module Universum.Function
       , module Universum.Functor
       , module Universum.Lifted
       , module Universum.List
       , module Universum.Monad
       , module Universum.Monoid
       , module Universum.Nub
       , module Universum.Print
       , module Universum.String
       , module Universum.TypeOps
       , module Universum.VarArg

         -- * Lenses
       , Lens
       , Lens'
       , Traversal
       , Traversal'
       , over
       , set
       , (%~)
       , (.~)
       , (^.)
       , (^..)
       , (^?)
       , _1
       , _2
       , _3
       , _4
       , _5
       , preuse
       , preview
       , use
       , view
       ) where

import Universum.Applicative
import Universum.Base
import Universum.Bool
import Universum.Container
import Universum.Debug
import Universum.DeepSeq
import Universum.Exception
import Universum.Function
import Universum.Functor
import Universum.Lifted
import Universum.List
import Universum.Monad
import Universum.Monoid
import Universum.Nub
import Universum.Print
import Universum.String
import Universum.TypeOps
import Universum.VarArg

-- Lenses
import qualified Lens.Micro (ASetter, Getting, over, set, (%~), (.~), (^.),
                             (^..), (^?), _1, _2, _3, _4, _5)
import qualified Lens.Micro.Mtl (preuse, preview, use, view)
import Lens.Micro.Internal (Field1, Field2, Field3, Field4, Field5)

{-# DEPRECATED
    Lens
  , Lens'
  , Traversal
  , Traversal'
  , over
  , set
  , (%~)
  , (.~)
  , (^.)
  , (^..)
  , (^?)
  , _1
  , _2
  , _3
  , _4
  , _5
  , preuse
  , preview
  , use
  , view
  "Use corresponding function from 'lens' or 'microlens' package"
#-}

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

over :: Lens.Micro.ASetter s t a b -> (a -> b) -> s -> t
over = Lens.Micro.over

set :: Lens.Micro.ASetter s t a b -> b -> s -> t
set = Lens.Micro.set

(%~) :: Lens.Micro.ASetter s t a b -> (a -> b) -> s -> t
(%~) = (Lens.Micro.%~)

infixr 4 %~

(.~) :: Lens.Micro.ASetter s t a b -> b -> s -> t
(.~) = (Lens.Micro..~)

infixr 4 .~

(^.) :: s -> Lens.Micro.Getting a s a -> a
(^.) = (Lens.Micro.^.)

infixl 8 ^.

(^..) :: s -> Lens.Micro.Getting (Endo [a]) s a -> [a]
(^..) = (Lens.Micro.^..)

infixl 8 ^..

(^?) :: s -> Lens.Micro.Getting (First a) s a -> Maybe a
(^?) = (Lens.Micro.^?)

infixl 8 ^?

_1 :: Field1 s t a b => Lens s t a b
_1 = Lens.Micro._1

_2 :: Field2 s t a b => Lens s t a b
_2 = Lens.Micro._2

_3 :: Field3 s t a b => Lens s t a b
_3 = Lens.Micro._3

_4 :: Field4 s t a b => Lens s t a b
_4 = Lens.Micro._4

_5 :: Field5 s t a b => Lens s t a b
_5 = Lens.Micro._5

preuse :: MonadState s m => Lens.Micro.Getting (First a) s a -> m (Maybe a)
preuse = Lens.Micro.Mtl.preuse

preview :: MonadReader s m => Lens.Micro.Getting (First a) s a -> m (Maybe a)
preview = Lens.Micro.Mtl.preview

use :: MonadState s m => Lens.Micro.Getting a s a -> m a
use = Lens.Micro.Mtl.use

view :: MonadReader s m => Lens.Micro.Getting a s a -> m a
view = Lens.Micro.Mtl.view
