{-# options -fno-warn-orphans -XOverloadedStrings #-}
module LZ77 where

import FlatParse.Basic qualified as Flat
import Data.ByteString.Builder qualified as B
import Data.ByteString qualified as S
import Data.ByteString qualified as L
import Data.ByteString (ByteString)
import Test.QuickCheck

--------------------------------------------------------------------------------
-- Reference implementation: inefficient, but trivial

encode_ref :: ByteString -> ByteString
encode_ref = maybe (error "impossible") id . resultToMaybe . Flat.runParser p where
  p = fmap (L.toStrict . B.toLazyByteString . mconcat) $ Flat.many step
  step = do
    w8 <- Flat.anyWord8
    pure $ constant w8
  constant c = "\x00\x00" <> B.word8 c

decode_ref :: ByteString -> Flat.Result () ByteString
decode_ref = Flat.runParser (fmap S.pack $ Flat.many p) where
  p = do
    _back <- Flat.word8 0x00
    _len <- Flat.word8 0x00
    c <- Flat.anyWord8
    pure c

-- Property: decode is a _retraction_ of encode
retraction_prop :: ByteString -> Bool
retraction_prop = \x -> resultToMaybe (decode_ref (encode_ref x)) == Just x

--------------------------------------------------------------------------------
-- Test helpers

resultToMaybe :: Flat.Result () a -> Maybe a
resultToMaybe (Flat.OK a "") = Just a
resultToMaybe _ = Nothing

instance Arbitrary ByteString where
  arbitrary = fmap S.pack arbitrary
