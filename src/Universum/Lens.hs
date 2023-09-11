{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe       #-}

-- | Operators and functions compatible with lens constructed with
-- any of `lens` and `microlens` packages. Both those packages contain
-- operators, types, and functions defined here, so you can find all related
-- documentation there.

module Universum.Lens
       ( Lens
       , Lens'
       , Traversal
       , Traversal'

       , ASetter
       , Getting

       , over
       , set
       , get
       , (%~)
       , (.~)
       , (^.)
       , (^..)
       , (^?)
       ) where


import Data.Maybe (Maybe (..))
import Universum.Applicative
import Universum.Function
import Universum.Functor
import Universum.Monoid

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

type ASetter s t a b = (a -> Identity b) -> s -> Identity t

type Getting r s a = (a -> Const r a) -> s -> Const r s

over :: ASetter s t a b -> (a -> b) -> s -> t
over setter f = runIdentity . setter (Identity . f)

set :: ASetter s t a b -> b -> s -> t
set setter f = over setter (const f)

get :: s -> Getting a s a -> a
get s getter = getConst $ getter Const s

(%~) ::ASetter s t a b -> (a -> b) -> s -> t
(%~) = over

infixr 4 %~

(.~) :: ASetter s t a b -> b -> s -> t
(.~) = set

infixr 4 .~

(^.) :: s -> Getting a s a -> a
(^.) = get

infixl 8 ^.

(^..) :: s -> Getting (Endo [a]) s a -> [a]
s ^.. getter =
  let endo = getConst $ getter (\x -> Const $ Endo (x :)) s in
  appEndo endo []

infixl 8 ^..

(^?) :: s -> Getting (First a) s a -> Maybe a
s ^? getter = getFirst $ getConst $ getter (Const . pure) s

infixl 8 ^?
