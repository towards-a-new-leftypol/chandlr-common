{-# LANGUAGE BangPatterns #-}

module Common.BitField
    ( bitFieldFromInts
    , intsFromBitField
    )
where

import Data.Bits

import Data.Set (Set)
import qualified Data.Set as S

bitFieldFromInts :: Set Int -> Integer
bitFieldFromInts = bitFieldFromIntsDesc . S.toDescList

intsFromBitField :: Integer -> Set Int
intsFromBitField = S.fromList . f
    where
        f :: Integer -> [ Int ]
        f x
            | x == 0    = []
            | x < 0     = error "intsFromBitField: negative Integer not supported"
            | otherwise = tz : f (x .&. (x - 1))
            where
                tz = popCount ((x .&. (-x)) - 1)


-- | An optimized version that strictly requires a DESCENDING list of unique Ints.
-- Best paired with `Data.Set.toDescList`.
--
-- Instead of generating a bit mask for every element, this maintains a rolling
-- accumulator and shifts it by the difference between consecutive elements.
bitFieldFromIntsDesc :: [Int] -> Integer
bitFieldFromIntsDesc [] = 0
bitFieldFromIntsDesc (n:ns) = shiftL acc lastPos
    where
        (acc, lastPos) = go 1 n ns

        go :: Integer -> Int -> [Int] -> (Integer, Int)
        go !acc_ !pos [] = (acc_, pos)
        go !acc_ !pos (x:xs) =
            let diff = pos - x
            in go (shiftL acc_ diff + 1) x xs
