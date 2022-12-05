module Day.Five (part1, part2) where
import Util (chunksOf)
import Data.List (transpose, foldl')
import Data.Char (isSpace, isDigit)
import Data.Maybe (listToMaybe, mapMaybe)

part1 :: String -> String
part1 = concatMap (take 1) . applyMoves True . break ((=='1') . (!!1)) . lines

part2 :: String -> String
part2 = concatMap (take 1) . applyMoves False . break ((=='1') . (!!1)) . lines

applyMoves :: Bool -> ([String], [String]) -> [[Char]]
applyMoves rev (x,y) = foldl' (move rev) (parseStack x) (parseInstructions $ drop 2 y)

parseStack :: [String] -> [[Char]]
parseStack = map (dropWhile isSpace) . transpose . map (map (!!1) . chunksOf 4)

parseInstructions :: [String] -> [(Int,Int,Int)]
parseInstructions = map ((\[_,a,_,b,_,c] -> (a, b-1, c-1)) . map read . words)

move :: Bool -> [[Char]] -> (Int,Int,Int) -> [[Char]]
move rev ss (n,f,t) =
    let (ass,bs:bss) = splitAt f ss
        (mov,rem) = splitAt n bs
        (css,ds:dss) = splitAt t (ass ++ rem:bss)
    in css ++ ((if rev then reverse mov else mov) ++ds):dss




