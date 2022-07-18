{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Universum.UtfProps
  ( hprop_StringToBytes
  , hprop_TextToBytes
  , hprop_BytesTo
  ) where

import Universum

import Hedgehog (Gen, MonadGen, Property, assert, forAll, property)
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

utf8Text :: Gen T.Text
utf8Text = Gen.text (Range.linear 0 10000) unicode'

utf8Bytes :: Gen B.ByteString
utf8Bytes = Gen.utf8 (Range.linear 0 10000) unicode'

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
