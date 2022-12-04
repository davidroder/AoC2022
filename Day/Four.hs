module Day.Four (part1, part2) where
import Util (split)
import Data.Ix (inRange, rangeSize)
import Data.List (sortOn)

part1 :: String -> String
part1 = countParsed $ (\[s,l] -> all (inRange (tuple l)) s) . sortOn (rangeSize . tuple)

part2 :: String -> String
part2 = countParsed $ (\[s,l] -> any (inRange (tuple l)) s) . sortOn (rangeSize . tuple)

countParsed :: ([[Int]] -> Bool) -> String -> String
countParsed f = show . length . filter (f . map (map read . split (=='-')) . split (==',')) . lines

tuple :: [a] -> (a,a)
tuple [x,y] = (x,y)