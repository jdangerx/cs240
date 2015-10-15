{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BigChar where

import Data.Char (chr, ord)
import Data.Word (Word16)
import Data.Bits ((.&.), shiftR)

import qualified System.Random as R
import qualified Test.QuickCheck as QC

newtype BigChar =
  Big Char deriving (Show, Eq, R.Random)

instance QC.Arbitrary BigChar where
  arbitrary = QC.choose (Big '\0', Big '\x10FFFF')
  shrink (Big c) = map Big (shrinkChar c)

shrinkChar :: Char -> String
shrinkChar x
  | w < 0x10000 = chr <$> [fromIntegral w - 100]
  | otherwise   = chr <$> [fromIntegral a, fromIntegral b - 100]
  where w = ord x
        a = ((w - 0x10000) `shiftR` 10) + 0xD800
        b = (w .&. 0x3FF) + 0xDC00

encodeChar :: Char -> [Word16]
encodeChar x
  | w < 0x10000 = [fromIntegral w]
  | otherwise   = [fromIntegral a, fromIntegral b]
  where w = ord x
        a = ((w - 0x10000) `shiftR` 10) + 0xD800
        b = (w .&. 0x3FF) + 0xDC00

prop_encodeOne3 :: BigChar -> Bool
prop_encodeOne3 (Big c) = length (encodeChar c) == 1


