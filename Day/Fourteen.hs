{-# LANGUAGE LambdaCase #-}
module Day.Fourteen (part1, part2) where
import Util (split)
import Text.ParserCombinators.ReadP (ReadP, sepBy, string, munch1, char, readP_to_S, eof)
import Data.Char (isDigit)
import Data.Ix (Ix(range))
import qualified Data.Set as S
import Control.Monad.State.Lazy (State, get, modify, runState, evalState)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad (guard)
import Control.Applicative (Alternative((<|>), empty))
import Data.Functor (($>))
import Data.Bool (bool)
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader(ask))


part1 :: String -> String
part1 = show . (evalState =<< 
    runReaderT (countSand 0) . (False,) . fst . S.findMax
    ) . parsePathSet

part2 :: String -> String
part2 = show . (evalState =<< 
    runReaderT (countSand 0) . (True,) . (+2) . fst . S.findMax
    ) . parsePathSet

parsePathSet :: String -> S.Set (Int,Int)
parsePathSet = S.fromList . concat . 
    concatMap ((zipWith (\a b -> range (min a b, max a b)) =<< tail) .
    fst . head . readP_to_S parsePath) . lines

parsePath :: ReadP [(Int,Int)]
parsePath = (flip (,) <$> parseInt <* char ',' <*> parseInt) 
    `sepBy` string " -> " 
    <* eof

parseInt :: (Integral a, Read a) => ReadP a
parseInt = read <$> munch1 isDigit

countSand :: Int -> ReaderT (Bool,Int) (State (S.Set (Int,Int))) Int
countSand n = runMaybeT (placeSand (0,500)) >>= \case
    Just True   -> countSand (n+1)
    _           -> return n

placeSand
    :: (Int,Int) 
    -> MaybeT (ReaderT (Bool,Int) (State (S.Set (Int,Int)))) Bool
placeSand (y,x) = do
    s <- get
    guard $ S.notMember (y,x) s
    (floor, m) <- ask
    if y >= m
    then
        if floor 
        then empty 
        else return False
    else placeSand (y+1,x) <|>
        placeSand (y+1,x-1) <|>
        placeSand (y+1,x+1) <|>
        modify (S.insert (y,x)) $> True