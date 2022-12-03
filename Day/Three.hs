module Day.Three where
import Util (chunksOf)
import Data.List (find, intersect)
import Data.Char (isLower, isUpper)


priority :: Char ->  Int
priority c = (fromEnum c - 38) `mod` 58

part1 :: String -> String
part1 = show . sum . map (priority . head . uncurry intersect . (splitAt =<< (`div` 2) . length)) . lines

part2 :: String -> String
part2 = show . sum . map (priority . head . foldl1 intersect) . chunksOf 3 . lines