{-# LANGUAGE TypeApplications #-}
module Day.Thirteen (part1, part2) where
import Text.Read ( Read(readPrec), ReadPrec, (<++), readMaybe )
import Util (split)
import Data.Bool (bool)
import Data.List (sort, findIndices)
import Data.Maybe (mapMaybe) 

part1 :: String -> String
part1 = show . sum . zipWith (bool 0) [1..] . map ((\[x,y] -> x <= y) .
    map (read @LI)) . split null . lines

part2 :: String -> String
part2 = show . product . map (+1) . findIndices (`elem` divPackets) . 
    sort . (divPackets ++) . mapMaybe (readMaybe @LI) . lines

divPackets :: [LI]
divPackets = [L [L [I 6]], L [L [I 2]]]

data LI = L [LI] | I Int deriving Show

instance Read LI where 
    readPrec :: ReadPrec LI
    readPrec = (L <$> readPrec) <++ (I <$> readPrec)

instance Eq LI where
    (==) :: LI -> LI -> Bool
    L x == y@(I _)  = x == [y]
    x@(I _) == L y  = [x] == y
    L x == L y      = x == y
    I x == I y      = x == y

instance Ord LI where
    (<=) :: LI -> LI -> Bool
    L x <= y@(I _)  = x <= [y]
    x@(I _) <= L y  = [x] <= y
    L x <= L y      = x <= y
    I x <= I y      = x <= y
