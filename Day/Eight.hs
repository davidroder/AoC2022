{-# LANGUAGE TupleSections #-}
module Day.Eight (part1, part2) where
import Data.Array (Array, listArray, range, (!), bounds)
import Data.Char (digitToInt)



part1 :: String -> String
part1 = show . length . (filter . visible . toArray <*> range . arrayBounds) . lines


part2 :: String -> String
part2 = show . maximum . (map . viewDistance . toArray <*> range . arrayBounds) . lines

toArray :: [[Char]] -> Array (Int,Int) Int
toArray = listArray <$> arrayBounds <*> map digitToInt . concat

arrayBounds :: [[Char]] -> ((Int,Int), (Int,Int))
arrayBounds = curry ((0,0),) <$> pred . length . head <*> pred . length

visible :: Array (Int, Int) Int -> (Int, Int) -> Bool
visible a (x,y) = 
    let v = a!(x,y)
        ((_,_),(xMax,yMax)) = bounds a
        left = range ((0,y),(x-1,y))
        up = range ((x,0),(x,y-1))
        right = range ((x+1,y),(xMax,y))
        down = range ((x, y+1),(x,yMax))
    in  any (all ((<v).(a!))) [left,up,right,down]

viewDistance :: Array (Int, Int) Int -> (Int, Int) -> Int
viewDistance a (x,y) = 
    let v = a!(x,y)
        ((_,_),(xMax,yMax)) = bounds a
        left = reverse $ range ((0,y),(x-1,y))
        up = reverse $ range ((x,0),(x,y-1))
        right = range ((x+1,y),(xMax,y))
        down = range ((x, y+1),(x,yMax))
    in  product $ map (countVisible a v) [left,up,right,down]

countVisible :: Array (Int,Int) Int -> Int -> [(Int,Int)] -> Int
countVisible a v [] = 0
countVisible a v (x:xs)
    | (a!x) < v = 1 + countVisible a v xs
    | otherwise = 1