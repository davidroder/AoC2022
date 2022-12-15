{-# LANGUAGE MultiWayIf #-}
module Day.Fifteen (part1, part2) where
import Text.ParserCombinators.ReadP (ReadP, string, sepBy, char, eof, readP_to_S, skipSpaces)
import Text.ParserCombinators.ReadPrec (readPrec_to_P)
import Text.Read (Read(readPrec))
import Data.Ix (Ix (range, rangeSize))
import Control.Monad (guard, (<=<))
import Data.Sequence (Seq ((:|>), Empty, (:<|)), singleton, fromList, sort)
import Data.Foldable (find)

part2Range :: (Int, Int)
part2Range = (0,4000000)

part1 :: String -> String
part1 = show . sum . fmap rangeSize . foldl combine Empty . sort . 
    (beaconRange 2000000 <=< parseInp)

part2 :: String -> String
part2 s = show $ 
    (\(Just (y, (0,predX) :<| _)) -> (predX+1) * 4000000 + y) $
    find ((/= singleton part2Range) . snd) $
    fmap (\n -> 
        (n, foldl combine Empty (
            sort $ beaconRange2 part2Range n =<< parseInp s)
        )) (range part2Range)


parseInp :: String -> Seq ((Int,Int),(Int,Int))
parseInp s = fromList $ fst $ head $ flip readP_to_S s $ (do
    string "Sensor at x="
    x0 <- readPrec_to_P readPrec 0
    string ", y="
    y0 <- readPrec_to_P readPrec 0
    string ": closest beacon is at x="
    x1 <- readPrec_to_P readPrec 0
    string ", y="
    x2 <- readPrec_to_P readPrec 0
    return ((x0,y0),(x1,x2))
    ) `sepBy` char '\n' <* skipSpaces <* eof


beaconRange :: Int -> ((Int,Int),(Int,Int)) -> Seq (Int,Int)
beaconRange y ((sX,sY),(bX,bY)) =
    let dist = abs (bX-sX) + abs (bY-sY)
        rowDist = (dist - abs (sY - y))
        rangeStart = sX - rowDist
        rangeEnd = sX + rowDist
    in  if
        | bX == rangeStart -> singleton (rangeStart + 1, rangeEnd)
        | bX == rangeEnd -> singleton (rangeStart, rangeEnd - 1)
        | otherwise -> singleton (rangeStart, rangeEnd)

-- Assumes a0 < b0
combine :: Seq (Int, Int) -> (Int, Int) -> Seq (Int,Int)
combine (s :|> (a0,a1)) (b0,b1) = 
    if b0 <= a1 + 1 
    then s :|> (a0, max a1 b1) 
    else s:|>(a0,a1):|>(b0,b1)
combine Empty r = singleton r

beaconRange2 :: (Int,Int) -> Int -> ((Int,Int),(Int,Int)) -> Seq (Int,Int)
beaconRange2 (xMin,xMax) y ((sX,sY),(bX,bY)) =
    let dist = abs (bX-sX) + abs (bY-sY)
        rowDist = (dist - abs (sY - y))
        rangeStart = sX - rowDist
        rangeEnd = sX + rowDist
    in  guard (rangeStart <= xMax && rangeEnd >= xMin) *>
    singleton (max xMin rangeStart, min xMax rangeEnd)
