module Day.Five (part1, part2) where
import Util (chunksOf)
import Data.List (transpose, foldl')
import Data.Char (isSpace)
import Data.Array (listArray, Array, (!), (//))

part1 :: String -> String
part1 = concatMap (take 1) . applyMoves True . break ((=='1') . (!!1)) . lines

part2 :: String -> String
part2 = concatMap (take 1) . applyMoves False . break ((=='1') . (!!1)) . lines

applyMoves :: Bool -> ([String], [String]) -> Array Int [Char]
applyMoves rev (x,y) = foldl' (move rev) (parseStack x) (parseInstructions $ drop 2 y)

parseStack :: [String] -> Array Int [Char]
parseStack = (listArray =<< (,) 1 . length) . map (dropWhile isSpace) 
    . transpose . map (map (!!1) . chunksOf 4)

parseInstructions :: [String] -> [(Int,Int,Int)]
parseInstructions = map ((\[_,a,_,b,_,c] -> (a, b, c)) . map read . words)

move :: Bool -> Array Int [Char] -> (Int,Int,Int) -> Array Int [Char]
move rev ss (n,f,t) =
    let (mov,rem) = splitAt n (ss ! f)
        to = ss ! t
    in ss // [(f, rem), (t, (if rev then reverse mov else mov) ++ to)]
