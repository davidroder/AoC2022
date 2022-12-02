module Day.One (part1, part2) where
import Util (split)
import Data.List (sort)

part1 :: String -> String
part1 = show . maximum . map (sum . map read) . split null . lines

part2 :: String -> String
part2 = show . sum . take 3 . reverse . sort . map (sum . map read) . split null . lines