module Day.Six (part1,part2) where
import Data.List (transpose, nub, findIndex)

part1 :: String -> String
part1 = both 4

part2 :: String -> String
part2 = both 14

both :: Int -> String -> String
both n = show . (+n) . sum . findIndex ((==n) . length . nub) . transpose . ((<$> [0..n-1]) . flip drop)