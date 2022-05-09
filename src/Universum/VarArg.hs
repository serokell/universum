{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Safe                   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Provides operator of variable-arguments function composition.

module Universum.VarArg
       ( SuperComposition(..)
       ) where
import Data.Type.Bool (Not, type (||))
import Data.Type.Equality (type (==))
import Prelude (Bool (..))

-- $setup
-- >>> import Universum.Base ((+))
-- >>> import Universum.Container (null)
-- >>> import Prelude (show)
-- >>> import Data.List (zip5)

-- | This type class allows to implement variadic composition operator.
class SuperComposition x y b r | x y b -> r where
    -- | Applies a function to the result of another function with multiple
    -- arguments.
    --
    -- >>> (show ... (+)) (1 :: Int) 2
    -- "3"
    -- >>> (show ... (+)) 1 2 :: String
    -- "3"
    -- >>> show ... (5 :: Int)
    -- "5"
    -- >>> show ... 5 :: String
    -- "5"
    -- >>> (null ... zip5) [1] [2] [3] [] [5]
    -- True
    --
    -- Note that the type checker needs to have enough information on hand to deduce
    -- the appropriate arity for the second argument, which explains the need for explicit
    -- types in some examples above.
    --
    -- Inspired by <http://stackoverflow.com/questions/9656797/variadic-compose-function>.
    --
    -- ==== Performance
    -- To check the performance there was done a bunch of benchmarks. Benchmarks were made on
    -- examples given above and also on the functions of many arguments.
    -- The results are showing that the operator ('...') performs as fast as
    -- plain applications of the operator ('Prelude..') on almost all the tests, but ('...')
    -- leads to the performance draw-down if @ghc@ fails to inline it.
    -- Slow behavior was noticed on functions without type specifications.
    -- That's why keep in mind that providing explicit type declarations for functions is very
    -- important when using ('...').
    -- Relying on type inference will lead to the situation when all optimizations
    -- disappear due to very general inferred type. However, functions without type
    -- specification but with applied @INLINE@ pragma are fast again.
    --
    (...) :: (x -> y) -> b -> r

infixl 8 ...

-- The implementation is a bit tricky to get right. See #265 for how things can go wrong.
-- The basic idea is that we can commit to using the base case if we know we've reached
-- a result of the right type *or* we know that we don't have any arrows left. Similarly,
-- we can commit to using the recursive case if we know we don't yet have a result of the
-- right type *or* we know that we have more arrows we can use.

type family IsArrow b where
  IsArrow (_ -> _) = 'True
  IsArrow _ = 'False

-- | Can we use the base case?
type PlainApplication y b r = y == r || Not (IsArrow b)

-- | Can we use the recursive case?
type Composing y b r = Not (y == r) || IsArrow b

class SuperComposition' (plainApplication :: Bool) (composing :: Bool) x y b r | x y b -> r where
    comp :: (x -> y) -> b -> r

instance (x ~ b, y ~ r) =>
         SuperComposition' 'True composing x y b r where
    comp f = f
    {-# INLINE comp #-}

instance {-# INCOHERENT #-} (b ~ (b1 -> b'), r ~ (b1 -> r'), SuperComposition x y b' r') =>
         SuperComposition' plainApplication 'True x y b r where
    (f `comp` g) c = f ... g c
    {-# INLINE comp #-}

instance ( pa ~ PlainApplication y b r
         , co ~ Composing y b r
         , SuperComposition' pa co x y b r) =>
         SuperComposition x y b r where
    (...) = comp @pa @co
    {-# INLINE (...) #-}
