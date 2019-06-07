{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Bytes where
import Prelude hiding ((>>))
data Endian = Big | Little deriving(Eq, Ord)
data Interp = Signed | Unsigned deriving(Eq, Ord, Show)

instance Show Endian where
    show Little = "L hi<-lo"
    show Big = "B lo->hi"

newtype Width = Width Int deriving(Show, Eq, Ord, Num)

-- | list of bytes
data B = B [Bool] Endian Interp

instance Show B where
    show bs@(B xs endian i) =
      "[ix:0]" <>
      (map (\x -> if x then '1' else '0') xs) <>
      "[ix:" <> show ((length xs) - 1) <> "]" <>
      " " <>
      show endian <>
      " " <>
      show i <>
      " " <>
       "[" <> show (if i == Signed then int bs else uint bs) <> "]"



type Error = String

(//) :: Integer -> Integer -> Integer
(//) = div

(%) :: Integer -> Integer -> Integer
(%) = rem

ui2b :: Integer -> Width -> Endian -> B
ui2b i (Width w) e =
    if i >= 2 ^ w
    then error  ("integer: [" <> show i <>  "|log: " <> show ((log . fromInteger $ i) / (log . fromInteger $ 2)) <> "] greater that width: " <> show w)
    else
      let bits = flip map [0..(w-1)] $ \ix -> (i // (2 ^ ix)) % 2 == 1
       in B (if e == Little then bits else reverse bits) e Unsigned


i2b :: Integer -> Width -> Endian -> B
i2b i (Width w) e
    | i >= 2^(w-1) = error "integer greater than 2^(w-1) - 1"
    | i < -2^w = error "integer less than -2^w"
    | i >= 0 = let (B bs  _ _) = ui2b i (Width $ w - 1) Little
                   bits = False:bs
              in B (if e == Little then bits else reverse bits) e Signed
    | i < 0 = let (B bits _ _) = ui2b ((negate i) + 1) (Width $ w) Little
              in B (if e == Little then bits else reverse bits) e Signed

ui32l :: Integer -> B
ui32l i = ui2b i 32 Little

bytesUint64L :: Integer -> B
bytesUint64L i = ui2b i 64 Little

bytesUint128L :: Integer -> B
bytesUint128L i = ui2b i 128 Little

-- | Treat boolean as integer
boolAsInt :: Bool -> Integer
boolAsInt True = 1
boolAsInt False  = 0

-- | Convert sequence to unsigned integer
uint :: B -> Integer
uint (B bs Little _) = let w = length bs
  in sum (flip map [0..w -1] $ \ix -> (2^ix) * boolAsInt (bs !! ix))

-- | Convert sequence to signed integer
int :: B -> Integer
int b@(B bits Little s) =
    if last bits -- ^ highest bit = 1 => negative
    then ((-1) * uint b) + 1
    else uint (B  (init bits) Little s)

width :: B -> Width
width (B bs _ _) = Width . length $ bs

-- | Convert to little endian
little :: B -> B
little b@(B _ Little _) = b
little (B bs Big s) = B (reverse bs) Little s

-- | Convert to big endian
big :: B -> B
big b@(B _ Big _ ) = b
big (B bs Little s) = B (reverse bs) Big s


-- | Left shift
(<<) :: B -> Int -> B
(<<) (B bs Little s) n =
    let w = length bs
     in B (flip map [0..w-1] $ \i -> if (i - n) < 0 then False else bs !! (i - n)) Little s
(<<) b n = big $ little b << n

-- | Right shift
(>>) :: B -> Int -> B
(>>) (B bs Little s) n =
    let w = length bs
     in B (flip map [0..w-1] $ \i -> if (i + n) >= w then False else bs !! (i + n)) Little s
(>>) b n = big $ little b >> n

-- | Lift a bitwise function to a function for these
bitwise :: (Bool -> Bool -> Bool) -> B -> B -> B
bitwise f (B bs e s) (B bs' e' s')
  | e == e' && length bs == length bs' = B (zipWith f bs bs') e s -- ^ TODO: decide what to do when signs don't match
  | e /= e' = error "endian-nesses not equal"
  | otherwise = error "widths not equal"

(#&) :: B -> B -> B
(#&) = bitwise (&&)

(#|) :: B -> B -> B
(#|) = bitwise (||)

(#!) :: B -> B
(#!) (B bs e s) = B (map not bs) e s

(#^) :: B -> B -> B
(#^) = bitwise (\x y -> (x && not y) || (not x && y))

-- | Truncate to N bits, from least to most significant
trunc :: B -> Int -> B
trunc (B bs Little s) n = B (take n bs) Little s
trunc b n = big $ trunc (little b) n


-- | Slice bits [ixlow,.., ixhigh] and change the width to the new width
slice :: B -> (Int, Int) -> B
slice (B bs Little s) (lo, hi) =
    let len = hi - lo + 1
     in B (take len (drop lo bs)) Little s

-- | Get the Nth block of width W
block :: Width -> Int -> B -> B
block (Width w) ix b = slice b (w*ix, w*(ix+1) - 1)

-- | make a block signed
signed :: B -> B
signed (B bs e _) = B bs e Signed
