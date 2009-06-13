{-# OPTIONS -fglasgow-exts -w #-}
module Properties where

import Test.QuickCheck
import Graphics.Pgm
import System.Random
import Data.Word
import Data.Array.Unboxed
import Data.Array.IArray
import Control.Monad
import Text.Parsec
import Data.ByteString as B

instance Arbitrary (UArray (Int,Int) Word16) where
    arbitrary = do r <- choose (1,5)
                   c <- choose (1,5)
                   vs :: [Word16] <- liftM (Prelude.map (fromIntegral.abs)) $ (chooseN (1,65000) (r*c) :: Gen [Int])
                   return $ listArray ((0,0), (r,c)) vs

chooseN :: (Integral b, Random a) => (a, a) -> b -> Gen [a]
chooseN ranges n = do h <- choose ranges
                      rest <- chooseN ranges (n-1)
                      return (h : rest)

instance Eq ParseError where
    a == b = False

prop_readwrite_id (arr :: UArray (Int,Int) Word16) = pgmToArray (arrayToPgm arr) == Right arr


prop_readwrite_with_comment_id (arr :: UArray (Int,Int) Word16) (str :: String) =
    (pgmToArrayWithComments (arrayToPgmWithComment arr str)) == Right (arr, str)
