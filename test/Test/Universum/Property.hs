{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Universum.Property
  ( hprop_StringToTextAndBack
  , hprop_StringToTextAndBackSurrogate
  , hprop_TextToStringAndBack
  , hprop_StringToBytes
  , hprop_TextToBytes
  , hprop_BytesTo
  , hprop_ordNubCorrect
  , hprop_hashNubCorrect
  , hprop_sortNubCorrect
  , hprop_unstableNubCorrect
  , hprop_andM
  , hprop_orM
  ) where

import Universum

import Data.List (nub)
import Hedgehog (Gen, MonadGen, Property, assert, forAll, property, (===))
#if MIN_VERSION_hedgehog(1,0,0)
import Hedgehog (GenBase)
#endif

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Universum as U

#if MIN_VERSION_hedgehog(1,0,0)
unicode' :: (MonadGen m, GenBase m ~ Identity) => m U.Char
#else
unicode' :: MonadGen m => m U.Char
#endif
unicode' = do
    a <- Gen.unicode
    if U.elem a ['\65534', '\65535']
    then unicode'
    else return a

utf8String :: Gen U.String
utf8String = Gen.string (Range.linear 0 10000) unicode'

unicodeAllString :: Gen U.String
unicodeAllString = Gen.string (Range.linear 0 10000) Gen.unicodeAll

utf8Text :: Gen T.Text
utf8Text = Gen.text (Range.linear 0 10000) unicode'

unicodeAllText :: Gen T.Text
unicodeAllText = Gen.text (Range.linear 0 10000) Gen.unicodeAll

utf8Bytes :: Gen B.ByteString
utf8Bytes = Gen.utf8 (Range.linear 0 10000) unicode'

-- "\65534" fails, but this is from BU.toString
-- > import qualified Data.ByteString.UTF8 as BU
-- > BU.toString (BU.fromString "\65534") == "\65533"
-- > True

hprop_StringToTextAndBack :: Property
hprop_StringToTextAndBack = property $ do
  str <- forAll unicodeAllString
  toString (toText str) === str

-- | See comment to this function:
-- <http://hackage.haskell.org/package/text-1.2.3.1/docs/src/Data.Text.Internal.html#safe>
--
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
  -- Without rewrite rule this string would be transformed to "\9435"
  let str = "\xD800"
  toString (toText str) === str

hprop_TextToStringAndBack :: Property
hprop_TextToStringAndBack = property $ do
  txt <- forAll unicodeAllText
  toText (toString txt) === txt

hprop_StringToBytes :: Property
hprop_StringToBytes = property $ do
    str <- forAll utf8String
    assert $ str == (decodeUtf8 (encodeUtf8 str :: B.ByteString))
          && str == (decodeUtf8 (encodeUtf8 str :: LB.ByteString))


hprop_TextToBytes :: Property
hprop_TextToBytes = property $ do
    txt <- forAll utf8Text
    assert $ txt == (decodeUtf8 (encodeUtf8 txt :: B.ByteString))
          && txt == (decodeUtf8 (encodeUtf8 txt :: LB.ByteString))

-- "\239\191\190" fails, but this is the same as "\65534" :: String
hprop_BytesTo :: Property
hprop_BytesTo = property $ do
    utf <- forAll utf8Bytes
    assert $ utf == (encodeUtf8 (decodeUtf8 utf :: U.String))
          && utf == (encodeUtf8 (decodeUtf8 utf :: T.Text))
          && utf == (encodeUtf8 (decodeUtf8 utf :: LT.Text))

-- ordNub

genIntList :: Gen [U.Int]
genIntList = Gen.list (Range.linear 0 10000) Gen.enumBounded

hprop_ordNubCorrect :: Property
hprop_ordNubCorrect = property $ do
    xs <- forAll genIntList
    U.ordNub xs === nub xs

hprop_hashNubCorrect :: Property
hprop_hashNubCorrect = property $ do
    xs <- forAll genIntList
    U.hashNub xs === nub xs

hprop_sortNubCorrect :: Property
hprop_sortNubCorrect = property $ do
    xs <- forAll genIntList
    U.sortNub xs === (U.sort $ nub xs)

hprop_unstableNubCorrect :: Property
hprop_unstableNubCorrect = property $ do
    xs <- forAll genIntList
    (U.sort $ U.unstableNub xs) === (U.sort $ nub xs)


-- logicM
-- this section needs a little more thought

genBoolList :: Gen [U.Bool]
genBoolList = Gen.list (Range.linear 0 1000) Gen.bool

hprop_andM :: Property
hprop_andM = property $ do
    bs <- forAll genBoolList
    U.andM (return <$> bs) === ((return $ U.and bs) :: U.Maybe U.Bool)

hprop_orM :: Property
hprop_orM = property $ do
    bs <- forAll genBoolList
    U.orM (return <$> bs) === ((return $ U.or bs) :: U.Maybe U.Bool)
