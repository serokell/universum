-- Some tests in this module have a caveat: they depend on compiler
-- optimizations and will likely fail if compiled with -O0.
-- That's because they depend on rewrite rules (and test these rules),
-- and these rules are usually not triggered without certain optimizations.

module Test.Universum.StringProps
  ( hprop_StringToTextAndBack
  , hprop_StringToTextAndBackSurrogate
  , hprop_TextToStringAndBack
  ) where

import Universum

import qualified Data.Text as T
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Universum as U

unicodeAllString :: Gen U.String
unicodeAllString = Gen.string (Range.linear 0 10000) Gen.unicodeAll

unicodeAllText :: Gen T.Text
unicodeAllText = Gen.text (Range.linear 0 10000) Gen.unicodeAll

-- "\65534" fails, but this is from BU.toString
-- > import qualified Data.ByteString.UTF8 as BU
-- > BU.toString (BU.fromString "\65534") == "\65533"
-- > True

hprop_StringToTextAndBack :: Property
hprop_StringToTextAndBack = property $ do
  str <- forAll unicodeAllString
  toString (toText str) === str

-- |
-- While 'String' may contain surrogate UTF-16 code points, actually UTF-8
-- doesn't allow them, as well as 'Text'. 'Data.Text.pack' replaces invalid
-- characters with unicode replacement character, so by default
-- @toString . toText@ is not identity.
--
-- However, we have a rewrite rule by which we /replace/ @toString . toText@
-- occurrences with the identity function.
hprop_StringToTextAndBackSurrogate :: Property
hprop_StringToTextAndBackSurrogate = property $ do
  -- Surrogate character like this one should remain intact
  -- Without the rewrite rule this string would be transformed to some other,
  -- valid string
  let str = "\xD800"
  toString (toText str) === str

hprop_TextToStringAndBack :: Property
hprop_TextToStringAndBack = property $ do
  txt <- forAll unicodeAllText
  toText (toString txt) === txt
