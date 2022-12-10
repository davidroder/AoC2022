{-# LANGUAGE LambdaCase #-}
module Day.Ten (part1, part2) where
import Control.Monad.State.Lazy (modify, get, evalState)
import Control.Monad (foldM)
import Util (chunksOf)
import Data.Bool (bool)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

part1 :: String -> String
part1 = show . flip evalState 1 . foldM (\t (c,n) -> do
        x <- get
        modify (+n)
        return $ t + bool 0 (c*x) (((c-20) `mod` 40) == 0)
    ) 0 . zip [1..] . map (fromMaybe 0 . readMaybe) . words

part2 :: String -> String
part2 = unlines . chunksOf 40 . flip evalState 1 . mapM (\(p,n) ->
        bool '.' '#' . ((<= 1) . abs . subtract p) <$> get 
        <* modify (+n)
    ) . zip (cycle [0 .. 39]) . map (fromMaybe 0 . readMaybe) . words