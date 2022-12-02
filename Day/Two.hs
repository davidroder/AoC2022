module Day.Two where
import Data.Monoid (Sum(Sum))

part1 :: String -> String
part1 s = show $ sum [y - 87 + 3 * ((2 + y - x) `mod` 3) | [x,_,y] <- map (map fromEnum) $ lines s]

part2 :: String -> String
part2 s = show $ sum [3 * (y - 88) + 1 + ((y + x - 1) `mod` 3) | [x,_,y] <- map (map fromEnum) $ lines s]
-- Alternative:
-- part2 = show . foldMap (Sum . (\[x,_,y] -> 3 * (y - 88) + 1 + ((y + x - 1) `mod` 3)) . map fromEnum) . lines