{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Universum.Property
        ( hedgehogTestTree
        ) where

import Universum

import Data.List (nub)
import Hedgehog (Gen, MonadGen, Property, assert, forAll, property, (===))
#if MIN_VERSION_hedgehog(1,0,0)
import Hedgehog (GenBase)
#endif
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Universum as U

hedgehogTestTree :: TestTree
hedgehogTestTree = testGroup "Tests" [stringProps, utfProps, listProps, boolMProps]

stringProps :: TestTree
stringProps = testGroup "String conversions"
    [ testProperty "toString . toText = id" prop_StringToTextAndBack
    , testProperty "`toString . toText` for UTF-16 surrogate"
        prop_StringToTextAndBackSurrogate
    , testProperty "toText . toString = id" prop_TextToStringAndBack
    ]

utfProps :: TestTree
utfProps = testGroup "utf8 conversion property tests"
    [ testProperty "String to ByteString invertible" prop_StringToBytes
    , testProperty "Text to ByteString invertible" prop_TextToBytes
    , testProperty "ByteString to Text or String invertible" prop_BytesTo
    ]

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

prop_StringToTextAndBack :: Property
prop_StringToTextAndBack = property $ do
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
prop_StringToTextAndBackSurrogate :: Property
prop_StringToTextAndBackSurrogate = property $ do
  -- Surrogate character like this one should remain intact
  -- Without rewrite rule this string would be transformed to "\9435"
  let str = "\xD800"
  toString (toText str) === str

prop_TextToStringAndBack :: Property
prop_TextToStringAndBack = property $ do
  txt <- forAll unicodeAllText
  toText (toString txt) === txt

prop_StringToBytes :: Property
prop_StringToBytes = property $ do
    str <- forAll utf8String
    assert $ str == (decodeUtf8 (encodeUtf8 str :: B.ByteString))
          && str == (decodeUtf8 (encodeUtf8 str :: LB.ByteString))


prop_TextToBytes :: Property
prop_TextToBytes = property $ do
    txt <- forAll utf8Text
    assert $ txt == (decodeUtf8 (encodeUtf8 txt :: B.ByteString))
          && txt == (decodeUtf8 (encodeUtf8 txt :: LB.ByteString))

-- "\239\191\190" fails, but this is the same as "\65534" :: String
prop_BytesTo :: Property
prop_BytesTo = property $ do
    utf <- forAll utf8Bytes
    assert $ utf == (encodeUtf8 (decodeUtf8 utf :: U.String))
          && utf == (encodeUtf8 (decodeUtf8 utf :: T.Text))
          && utf == (encodeUtf8 (decodeUtf8 utf :: LT.Text))

-- ordNub

listProps :: TestTree
listProps = testGroup "list function property tests"
    [ testProperty "Hedgehog ordNub xs == nub xs" prop_ordNubCorrect
    , testProperty "Hedgehog hashNub xs == nub xs" prop_hashNubCorrect
    , testProperty "Hedgehog sortNub xs == sort $ nub xs" prop_sortNubCorrect
    , testProperty "Hedgehog sort $ unstableNub xs == sort $ nub xs" prop_unstableNubCorrect
    ]

genIntList :: Gen [U.Int]
genIntList = Gen.list (Range.linear 0 10000) Gen.enumBounded

prop_ordNubCorrect :: Property
prop_ordNubCorrect = property $ do
    xs <- forAll genIntList
    U.ordNub xs === nub xs

prop_hashNubCorrect :: Property
prop_hashNubCorrect = property $ do
    xs <- forAll genIntList
    U.hashNub xs === nub xs

prop_sortNubCorrect :: Property
prop_sortNubCorrect = property $ do
    xs <- forAll genIntList
    U.sortNub xs === (U.sort $ nub xs)

prop_unstableNubCorrect :: Property
prop_unstableNubCorrect = property $ do
    xs <- forAll genIntList
    (U.sort $ U.unstableNub xs) === (U.sort $ nub xs)


-- logicM
-- this section needs a little more thought

genBoolList :: Gen [U.Bool]
genBoolList = Gen.list (Range.linear 0 1000) Gen.bool

boolMProps :: TestTree
boolMProps = testGroup "lifted logic function property tests"
    [ testProperty "Hedgehog andM" prop_andM
    , testProperty "Hedgehog orM" prop_orM
    ]

prop_andM :: Property
prop_andM = property $ do
    bs <- forAll genBoolList
    U.andM (return <$> bs) === ((return $ U.and bs) :: U.Maybe U.Bool)

prop_orM :: Property
prop_orM = property $ do
    bs <- forAll genBoolList
    U.orM (return <$> bs) === ((return $ U.or bs) :: U.Maybe U.Bool)
