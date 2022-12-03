module CStrings (CString, emptyC, nullC, consC, headC, tailC, 
                 lengthC, (<!!>), (<++>), splitAtC,
                 encodeChar, decodeChar,
                 toString, fromString) where

import Data.Bits
import Data.Char
import Data.Array.IArray

--
-- Fast library for representing strings of up to 23 alpha characters
-- (a..z), plus space, -, and ' using just two integers.
-- Given (m, n) the low order 5 bits of n store the length of the
-- list. The rest can store up to 11 characters, encoded in 5 bits
-- each. m can store 12.
--

type CString = (Int, Int)

blockSize :: Int
blockSize 
  = 5

--
-- The point where we need to start using m.
-- 
splitPoint :: Int
splitPoint 
  = 11

--
-- All target bits are moved to the low end for extraction.
--
mask :: Int
mask
  = setBit 0 blockSize - 1

--
-- Useful for zeroing the top 4 bits.
--
bigMask :: Int
bigMask
  = setBit 0 ((splitPoint + 1) * blockSize) - 1

fromString :: String -> CString
fromString ""
  = emptyC
fromString (c : cs)
  = consC (encodeChar c) (fromString cs)
  
toString :: CString -> String
toString s
  = toString' 0
  where
    len = lengthC s
    toString' n
      | n == len = ""
      | otherwise = decodeChar (s <!!> n) : toString' (n + 1)
 
--
-- Useful for testing
--
showBitVector bv 0
  = ""
showBitVector bv n
  = showBitVector (bv `div` 2) (n - 1) ++ show (bv `mod` 2)

encodeChar :: Char -> Int
encodeChar c
  | c == ' '  = 26
  | c == '-'  = 27
  | c == '\'' = 28
  | otherwise = ord c - ord 'a'

decodeChar :: Int -> Char
decodeChar n
  | n == 26   = ' '
  | n == 27   = '-'  
  | n == 28   = '\'' 
  | otherwise = chr (n + ord 'a')

emptyC :: CString
emptyC 
  = (0, 0)

nullC :: CString -> Bool
nullC (_, n)
  = n == 0

--
-- The following save doing multiplications
--
mults :: Array Int Int
mults
  = array (0, splitPoint) (map f [0 .. splitPoint])
  where
    f n = (n, blockSize * n)

masks :: Array Int Int
masks
  = array (0, splitPoint) (map f [0 .. splitPoint])
  where
    f n = (n, bit (mults ! n) - 1)

shiftL' n k
  = shiftL n (k * blockSize) 

shiftR' n k
  = shiftR n (k * blockSize) 

--
-- Note that len is the length *before* adding c.
--
consC :: Int -> CString -> CString
consC c (m, n)
  | len < splitPoint = (m, (n .|. shiftc) + 1)
  | otherwise        = (m .|. shiftc', n + 1)
  where
    shiftc = shiftL' c (len + 1)
    shiftc' = shiftL' c (len - splitPoint)
    len = n .&. mask

headC :: CString -> Int
headC (m, n)
  | len <= splitPoint = shiftR' n len .&. mask
  | otherwise         = shiftR' m k .&. mask
  where
    len = n .&. mask
    k = len - splitPoint - 1

tailC :: CString -> CString
tailC (m, n)
  | len <= splitPoint     = (m, (n .&. (masks ! len)) - 1)
  | len == splitPoint + 1 = (0, n - 1)
  | otherwise             = (m .&. (masks ! k), n - 1)
  where
    len = n .&. mask
    k = len - splitPoint - 1

lengthC :: CString -> Int
lengthC (_, n)
  = n .&. mask

infixr 5 <!!>
(<!!>) :: CString -> Int -> Int
(m, n) <!!> k
  | k' <= splitPoint = shiftR' n k' .&. mask
  | otherwise        = shiftR' m k'' .&. mask
  where
    k' = (n .&. mask) - k
    k'' = k' - splitPoint - 1

infixr 5 <++>
(<++>) :: CString -> CString -> CString 
s@(m, n) <++> (m', n')
  | len' < splitPoint
    = (shiftL' m len' .|. shiftR' n (k + 1),
       ((n' .|. shiftL' (shiftR' n 1) (len' + 1)) + len) .&. bigMask)
  | otherwise -- m must be 0
    = (m' .|. shiftL' (shiftR' n 1) (-k), n' + len)
  where
    len = n .&. mask
    len' = n' .&. mask
    k = splitPoint - len'
 
--
-- Could do this recursively, but it's best to make
-- everthing constant time.
--
splitAtC :: Int -> CString -> (CString, CString)
splitAtC k s@(m, n)
  | k <= 0
    = (emptyC, s)
  | k >= len
    = (s, emptyC)
  | len <= splitPoint
    = ((0, shiftL' (shiftR' n (k' + 1)) 1 + k),
       (0, (n .&. (masks ! (len - k + 1))) - k))
  | k' < splitPoint
    = ((shiftR' m k', 
        ((shiftL' m k''' .|. shiftL' (shiftR' n (k' + 1)) 1) + k) .&. bigMask),
       (0, (n .&. masks ! (k' + 1)) - k))
  | otherwise
    = ((0, shiftL' (shiftR' m k'') 1 + k), (m .&. (masks ! k''), n - k))
  where
    len = n .&. mask
    k' = len - k
    k'' = len - splitPoint - k
    k''' = k - (len - splitPoint) + 1

   
