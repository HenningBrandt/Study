module IEEE754 (floatToDecimal) where 
import Data.Char

type Bits = [Int]

floatToDecimal :: String -> Float
floatToDecimal str = let bits = toBits str
                         f = factor bits
                         e = IEEE754.exponent bits
                         m = mantisse bits
                         (intP, fracP) = splitParts e m
                     in f * ((toF . bitsToInt $ intP) + (bitsToFrac $ fracP)) 


-- ##################################################################
-- Split bits into integer and fraction part based on the exponent
-- ##################################################################

-- split bits into integer and fraction part using a given exponent
splitParts :: Int -> Bits -> (Bits, Bits)
splitParts e bits = splitAt splitIdx bits'
    where
        splitIdx = if e > 0 then e + 1 else 1
        bits' = fillBits e bits

-- repeats a given value n-times
repeatN :: Int -> a -> [a]
repeatN n e = take n . repeat $ e

-- fill missing bits with zeros based on the offset defined by the exponent
fillBits :: Int -> Bits -> Bits
fillBits e bits
    | e > 0 = bits ++ repeatN holes 0
    | otherwise = (repeatN holes 0) ++ bits
    where 
        holes = if e > 0 then maximum [0, e - (length bits) + 2] else abs e


-- ##################################################################
-- Retrieving different parts of the IEEE754 Float
-- ##################################################################

-- Bias for exponent
cBIAS = 127
-- Bits for algebraic sign
cSIGN_BIT = 1
-- Bit lengths for exponent and mantisse
cEXP_BITS = 8
cMAN_BITS = 23

-- determines the algebraic sign
factor :: Bits -> Float
factor (0:_) = 1.0
factor _ = (-1.0)

exponent :: Bits -> Int
exponent bits = (bitsToInt . take cEXP_BITS . drop cSIGN_BIT $ bits) - cBIAS

mantisse :: Bits -> Bits
mantisse bits = (1 :) . take cMAN_BITS . drop (cSIGN_BIT + cEXP_BITS) $ bits


-- ##################################################################
-- Converting Types 
-- ##################################################################

-- Convert the bits represented as a string to actual numbers
toBits :: String -> Bits
toBits = map (\b -> ord b - 48)

-- Shorthand for fromIntegral
toF :: Int -> Float
toF = fromIntegral

-- Convert integer part from binary to decimal
bitsToInt :: Bits -> Int
bitsToInt xs = foldl1 (+) . map (\(w,b) -> w * b) . zip weights $ bits
    where
        weights = map (2^) [0..]
        bits = reverse xs

-- Convert fraction part from binary to decimal
bitsToFrac :: Bits -> Float
bitsToFrac xs = foldl1 (+) . map (\(w,b) -> w * b) . zip weights $ bits
    where
        weights = map ((1.0/) . (2.0**)) [1.0..]
        bits = map toF xs