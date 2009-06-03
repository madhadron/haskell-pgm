{-# OPTIONS_GHC -XScopedTypeVariables -XFlexibleContexts #-}

-- | "Graphics.Pgm" is a pure Haskell library to read and write PGM images.  It properly supports both 8 bit and 16 bit pixels, and multiple PGMs per file.  The PGM is the lowest common denominator of useful image file formats.  It consists of a header of the form
-- 
-- @P5 width height maxVal@
-- 
-- followed by a single whitespace charater, usually a newline, where @width@, @height@, and @maxVal@ are positive integers consisting of digits only giving the number of columns, number of rows, and the highest grey level in the image to follow.
-- 
-- If @maxVal@ < 256, then the format uses 1 byte per pixel; otherwise it uses 2.  The routines in this library properly handle both, including automatically determining which to write when writing an array to disk.
-- 
-- The header can also contain comments, starting with @#@ on a new line, and continuing to the end of the line.  These are ignored by this module.
-- 
-- After the header, the pixel data is written in big-endian binary form, most significant byte first for 16 bit pixels.  The pixels are a single row-major raster through the image.
-- 
-- To put multiple PGMs in a file, append them.  This module allows you to put white space between them, though this might choke other implementations.
-- 
-- All arrays returned by this library from PGMs have pixel type 'Int', since this is simply more useful for most purposes.  If you want to write a PGM back out, you must first coerce your pixel type to 'Word16'!  There are too many possibile ways of handling negative values, larger depths, or other things beyond the comprehension of 'Word16' to handle with a simple wrapper function.  If you know you have positive values less than 2^16, then you can coerce an array @arr@ to 'Word16' with
-- 
-- > amap (fromIntegral :: Int -> Word16) arr
-- 
-- The array's indices (of the form (row,column)) start at (0,0) and run to (@height@-1,@width@-1).

module Graphics.Pgm (pgmToArray, pgmsToArrays, pgmsFromFile, pgmsFromHandle, 
            arrayToPgm, arrayToFile, arrayToHandle, arraysToHandle,
             arraysToFile) where

import Text.Parsec
import Text.Parsec.ByteString (Parser)
import System.IO
import Data.Array.Unboxed
import Data.ByteString as B (take, drop, unpack, pack, ByteString, 
                             append, hGetContents, hPutStr)
import Data.ByteString.Internal (c2w)
import Data.Word
import Text.Printf
import Control.Monad (liftM)

magicNumber :: Parser ()
magicNumber = do { char 'P'; char '5'; return () }

integer :: Parser Int
integer = do { s <- many1 digit; return $ read s }

width = integer
height = integer

maxVal = do { i <- integer; return $ min 65536 i }

comment :: Parser ()
comment = do { char '#'; manyTill anyChar (try newline); return () }

commentAwareWhiteSpace = many1 (choice [comment,do { many1 space; return () }])

pgmHeader :: Parser (Int,Int,Int)
pgmHeader = do magicNumber <?> "magic number"
               commentAwareWhiteSpace
               cols <- width <?> "width"
               commentAwareWhiteSpace
               rows <- height <?> "height"
               commentAwareWhiteSpace
               m <- maxVal <?> "maximum grey value"
               space
               return (rows,cols,m)

pgm :: Parser (UArray (Int,Int) Int)
pgm = do (rows,cols,m) <- pgmHeader
         let d = if (m < 256) then 1 else 2
         ip <- getInput
         let body = B.take (rows*cols*d) ip
         setInput $ B.drop (rows*cols*d) ip
         let arr = readArray d rows cols body
         return arr

pgms = many1 (do { h <- pgm ; spaces ; return h })


-- | Parse the first (and possible only) PGM in a 'ByteString' into an array.  If the parsing succeeds, you will still need to match on the 'Right' constructor to get the array.
pgmToArray :: B.ByteString -> Either ParseError (UArray (Int,Int) Int)
pgmToArray s = parse pgm "Failed to parse PGM." s

-- | Precisely the same as 'pgmToArray', but this time fetches all the PGMs in the file, and returns them as a list of arrays.
pgmsToArrays :: B.ByteString -> Either ParseError [UArray (Int,Int) Int]
pgmsToArrays s = parse pgms "Failed to parse PGMs." s

-- | A wrapper around 'pgmsFromHandle' which also opens the file to read from.
pgmsFromFile :: String -> IO (Either ParseError [UArray (Int,Int) Int])
pgmsFromFile fname = do h <- openFile fname ReadMode
                        s <- pgmsFromHandle h
                        hClose h
                        return s

-- | Parse all PGMs in the contents of a handle, and return them as a list of arrays.
pgmsFromHandle :: Handle -> IO (Either ParseError [UArray (Int,Int) Int])
pgmsFromHandle h = liftM pgmsToArrays $ B.hGetContents h


readArray8 :: Int -> Int -> B.ByteString -> UArray (Int,Int) Word8
readArray8 rows cols src = listArray ((0,0), (rows-1,cols-1)) (unpack src)

readArray16 :: Int -> Int -> B.ByteString -> UArray (Int,Int) Word16
readArray16 rows cols src = listArray ((0,0), (rows-1,cols-1)) src'
    where raw = unpack src
          src' = pairWith f raw
          f a b = (fromIntegral a)*256 + (fromIntegral b)

readArray :: Int -> Int -> Int -> B.ByteString -> UArray (Int,Int) Int
readArray 1 rows cols src = amap fromIntegral $ readArray8 rows cols src
readArray 2 rows cols src = amap fromIntegral $ readArray16 rows cols src

pair [] = []
pair (a:[]) = []
pair (a:b:ls) = (a,b):(pair ls)

pairWith f ls = Prelude.map (\(a,b) -> f a b) $ pair ls

pgmHeaderString :: Int -> Int -> Word16 -> B.ByteString
pgmHeaderString rows cols maxVal = pack $ Prelude.map c2w $ 
                             printf "P5 %d %d %d\n" cols rows maxVal

-- | Takes an array (which must already be coerced to have element type 'Word16') and produces a 'ByteString' encoding that array as a PGM.
arrayToPgm :: IArray m Word16 => m (Int,Int) Word16 -> B.ByteString
arrayToPgm arr = pgmHeaderString rows cols maxVal `B.append`
                   listToByteString maxVal (elems arr)
    where (rows,cols) = (ymax-ymin,xmax-xmin)
          ((xmin,ymin),(xmax,ymax)) = bounds arr
          maxVal = arrayLift max arr


arrayLift :: (Ix i, IArray m a) => (a -> a -> a) -> m i a -> a
arrayLift f arr = Prelude.foldl f (head q) q 
    where q = elems arr
          
listToByteString :: Word16 -> [Word16] -> B.ByteString
listToByteString d vs
    | d < 256 = pack $ ((Prelude.map fromIntegral vs)::[Word8])
    | otherwise = pack $ concat $ map (\x -> [fromIntegral (x `div` 256), 
                                              fromIntegral (x `rem` 256)]) vs

-- | Write a single array to a given handle.
arrayToHandle :: IArray m Word16 => Handle -> m (Int,Int) Word16 -> IO ()
arrayToHandle h arr = B.hPutStr h (arrayToPgm arr)

-- | A wrapper around 'arrayToHandle' which opens the file to write to, then closes it afterwards.
arrayToFile :: IArray m Word16 => String -> m (Int,Int) Word16 -> IO ()
arrayToFile fname arr = do h <- openFile fname WriteMode
                           arrayToHandle h arr
                           hClose h

-- | Writes a list of arrays to a given handle.  Note that most implementations of PGM will ignore all but the first when they read this file.
arraysToHandle :: IArray m Word16 => Handle -> [m (Int,Int) Word16] -> IO ()
arraysToHandle h arrs = mapM_ (arrayToHandle h) arrs

-- | A wrapper around 'arraysToHandle' which opens and closes the file to write to.
arraysToFile :: IArray m Word16 => String -> [m (Int,Int) Word16] -> IO ()
arraysToFile fname arrs = do h <- openFile fname WriteMode
                             arraysToHandle h arrs
                             hClose h