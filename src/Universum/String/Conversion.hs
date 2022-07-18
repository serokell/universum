{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy           #-}
{-# OPTIONS_GHC -Wno-orphans  #-}

-- | This module implements type class which allow to have conversion to and
-- from 'Text', 'String' and 'ByteString' types (including both strict and lazy
-- versions). Usually you need to export 'Text' modules qualified and use
-- 'T.pack' \/ 'T.unpack' functions to convert to\/from 'Text'. Now you can
-- just use 'toText' \/ 'toString' functions.

module Universum.String.Conversion
       ( -- * Convenient type aliases
         LText
       , LByteString

         -- * Conversion type classes
       , ConvertUtf8 (..)
       , ToString (..)
       , ToLText (..)
       , ToText (..)

         -- * Show and read functions
       , readEither
       , readMaybe
       , show
       ) where

import Data.Bifunctor (first)
import Data.Either (Either)
import Data.Function (id, (.))
import Data.Maybe (Maybe)
import Data.String (String)
import qualified Data.Text.Internal as T
import qualified Data.Text.Internal.Fusion.Common as TF

import Universum.Functor ((<$>))
import Universum.String.Reexport (ByteString, IsString, Read, Text, fromString)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LBU
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Text.Read (readEither, readMaybe)

import qualified GHC.Show as Show (Show (show))

-- $setup
-- >>> :set -XTypeApplications -XOverloadedStrings
-- >>> import Universum.Base (Int)
-- >>> import Universum.Function (($))
-- >>> import Universum.Print (putStrLn)

-- | Type synonym for 'Data.Text.Lazy.Text'.
type LText = LT.Text

-- | Type synonym for 'Data.ByteString.Lazy.ByteString'.
type LByteString = LB.ByteString


-- | Type class for conversion to utf8 representation of text.
class ConvertUtf8 a b where
    -- | Encode as utf8 string (usually 'B.ByteString').
    --
    -- >>> encodeUtf8 @Text @ByteString "патак"
    -- "\208\191\208\176\209\130\208\176\208\186"
    encodeUtf8 :: a -> b

    -- | Decode from utf8 string.
    --
    -- >>> decodeUtf8 @Text @ByteString "\208\191\208\176\209\130\208\176\208\186"
    -- "\1087\1072\1090\1072\1082"
    -- >>> putStrLn $ decodeUtf8 @Text @ByteString "\208\191\208\176\209\130\208\176\208\186"
    -- патак
    decodeUtf8 :: b -> a

    {- | Decode as utf8 string but returning execption if byte sequence is malformed.

#if MIN_VERSION_text(1,2,3)
    >>> decodeUtf8 @Text @ByteString "\208\208\176\209\130\208\176\208\186"
    "\65533\1072\1090\1072\1082"
#else
    >>> decodeUtf8 @Text @ByteString "\208\208\176\209\130\208\176\208\186"
    "\65533\65533\1090\1072\1082"
#endif
    >>> decodeUtf8Strict @Text @ByteString "\208\208\176\209\130\208\176\208\186"
    Left Cannot decode byte '\xd0': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream
    -}
    decodeUtf8Strict :: b -> Either T.UnicodeException a

instance ConvertUtf8 String B.ByteString where
    encodeUtf8 = BU.fromString
    decodeUtf8 = BU.toString
    decodeUtf8Strict = (T.unpack <$>) . decodeUtf8Strict

instance ConvertUtf8 T.Text B.ByteString where
    encodeUtf8 = T.encodeUtf8
    decodeUtf8 = T.decodeUtf8With T.lenientDecode
    decodeUtf8Strict = T.decodeUtf8'

instance ConvertUtf8 LT.Text B.ByteString where
    encodeUtf8 = LB.toStrict . encodeUtf8
    decodeUtf8 = LT.decodeUtf8With T.lenientDecode . LB.fromStrict
    decodeUtf8Strict = decodeUtf8Strict . LB.fromStrict

instance ConvertUtf8 String LB.ByteString where
    encodeUtf8 = LBU.fromString
    decodeUtf8 = LBU.toString
    decodeUtf8Strict = (T.unpack <$>) . decodeUtf8Strict

instance ConvertUtf8 T.Text LB.ByteString where
    encodeUtf8 = LB.fromStrict . T.encodeUtf8
    decodeUtf8 = T.decodeUtf8With T.lenientDecode . LB.toStrict
    decodeUtf8Strict = T.decodeUtf8' . LB.toStrict

instance ConvertUtf8 LT.Text LB.ByteString where
    encodeUtf8 = LT.encodeUtf8
    decodeUtf8 = LT.decodeUtf8With T.lenientDecode
    decodeUtf8Strict = LT.decodeUtf8'

-- | Type class for converting other strings to 'T.Text'.
class ToText a where
    toText :: a -> T.Text

instance ToText String where
    toText = T.pack

instance ToText T.Text where
    toText = id

instance ToText LT.Text where
    toText = LT.toStrict

-- | Type class for converting other strings to 'LT.Text'.
class ToLText a where
    toLText :: a -> LT.Text

instance ToLText String where
    toLText = LT.pack

instance ToLText T.Text where
    toLText = LT.fromStrict

instance ToLText LT.Text where
    toLText = id

-- | Type class for converting other strings to 'String'.
class ToString a where
    toString :: a -> String

instance ToString String where
    toString = id

instance ToString T.Text where
    toString = T.unpack

instance ToString LT.Text where
    toString = LT.unpack

{-

@toString . toText@ pattern may occur quite often after inlining because
we tend to use 'Text' rather than 'String' in function signatures, but
there are still some libraries which use 'String's and thus make us perform
conversions back and forth.

Note that @toString . toText@ is not strictly equal to identity function, see
explanation in the comment below.
-}

{-# RULES "pack/unpack" [~0]
    forall s. T.unpack (T.pack s) = s
#-}

{- [Note toString-toText-rewritting]

We can do even better than above if take rules defined in 'Data.Text' into
account.

Note ON MAINTENANCE: whenever you need to update the used version of @text@ package,
you have to check whether the comment below is still valid.
However, you can cut down by looking at the diff between versions of @text@, and
seeing if implementation of any of these functions have changed:
  * pack
  * unpack
  * streamList
  * unstreamList
  * safe
  * any RULES definition (if some is added, this counts)
If none of mentioned have changed, then it is safe to assume that everything
is still fine.

Quoting investigation of @int-index:

If we look at @unpack@ and @pack@ they are defined as

@
unpack = S.unstreamList . stream
{-# INLINE [1] unpack #-}

pack = unstream . S.map safe . S.streamList
{-# INLINE [1] pack #-}
@

After they get inlined, the rule seems to be

@
(S.unstreamList . stream) ((unstream . S.map safe . S.streamList) a)
@

If we also inline function composition, we get

@
S.unstreamList (stream (unstream (S.map safe (S.streamList a))))
@

`stream` and `unstream` surely cancel out via this rule:

@
{-# RULES "STREAM stream/unstream fusion" forall s. stream (unstream s) = s #-}
@

So we are left with

@
S.unstreamList (S.map safe (S.streamList a))
@

Now, what's this 'safe' function? Turns out it's defined as

@
safe :: Char -> Char
safe c
    | ord c .&. 0x1ff800 /= 0xd800 = c
    | otherwise                    = '\xfffd'
@

Aha, so it's mapping some codepoints to @'\xfffd'@!
There's a comment on top of it to explain this:

```
-- UTF-16 surrogate code points are not included in the set of Unicode
-- scalar values, but are unfortunately admitted as valid 'Char'
-- values by Haskell.  They cannot be represented in a 'Text'.  This
-- function remaps those code points to the Unicode replacement
-- character (U+FFFD, \'&#xfffd;\'), and leaves other code points
-- unchanged.
```

This logic is lost with the mentioned rewrite rule.
Not a huge loss, but it does mean that this rewrite rule isn't meaning preserving.


We hope that in most cases it's fine.
And if it's not, one can mark his function using either @pack@ or @unpack@
with @NOINLINE@ pragma to prevent the rule from firing.

So, eventually, we add the following rule:
-}
{-# RULES "pack/unpack internal" [1]
    forall s. TF.unstreamList (TF.map T.safe (TF.streamList s)) = s
#-}

{- In case if GHC didn't manage to inline and rewrite everything in
the remaining phases (@Data.Text.pack@ is inlined at 1-st phase),
we still have "pack/unpack" rule. Hopefully, one of them will fire.
-}

{- The opposite rule is safe to have because 'T.safe' /is/ the identity
function for strings made up from valid characters, and 'Text' is guaranteed
to have only valid ones.
However, for this case there is no @unstream (stream s) = id@ rule,
so we don't delve deep into internals. As long as @stream@ and @unstream@
only perform conversion between text and stream of characters, they should
be safe to collapse.
-}
{-# RULES "unpack/pack" [~0]
    forall s. T.pack (T.unpack s) = s
#-}

-- | Polymorhpic version of 'Text.Read.readEither'.
--
-- >>> readEither @Text @Int "123"
-- Right 123
-- >>> readEither @Text @Int "aa"
-- Left "Prelude.read: no parse"
--
-- @since 1.8.0
readEither :: (ToString a, Read b) => a -> Either Text b
readEither = first toText . Text.Read.readEither . toString

-- | Polymorhpic version of 'Text.Read.readMaybe'.
--
-- >>> readMaybe @Int @Text "123"
-- Just 123
-- >>> readMaybe @Int @Text "aa"
-- Nothing
readMaybe :: forall b a. (ToString a, Read b) => a -> Maybe b
readMaybe = Text.Read.readMaybe . toString

-- | Generalized version of 'Prelude.show'.
show :: forall b a . (Show.Show a, IsString b) => a -> b
show x = fromString (Show.show x)
{-# SPECIALIZE show :: Show.Show  a => a -> Text  #-}
{-# SPECIALIZE show :: Show.Show  a => a -> LText  #-}
{-# SPECIALIZE show :: Show.Show  a => a -> ByteString  #-}
{-# SPECIALIZE show :: Show.Show  a => a -> LByteString  #-}
{-# SPECIALIZE show :: Show.Show  a => a -> String  #-}
