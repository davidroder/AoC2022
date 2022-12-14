{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Day.Twelve (part1, part2) where
import Data.Bool (bool)
import Data.List (transpose, find, partition)
import Control.Monad.State.Strict (State, runState, get, modify, evalState, gets)
import qualified Data.IntMap.Strict as IM
import Control.Monad (when, forM_)
import Data.Bifunctor (Bifunctor(second))
import Data.Array (Array, array, listArray, (//), (!))

-- Very slow, particularly the brute force for part 2
-- May revisit and attempt to improve

part1 :: String -> String
part1 s = 
    let (indexed, l) = flip runState 1 $ 
            mapM (
                mapM (\a -> (,fromEnum a) <$> get <* modify (+1))
                ) $ lines s
        org = maybe 0 fst $ find ((==83) . snd) $ concat indexed
        dst = maybe 0 fst $ find ((==69) . snd) $ concat indexed
        indexed1 = map (map (\case
            (i,83) -> (i,fromEnum 'a')
            (i,69) -> (i,fromEnum 'z')
            x -> x)) indexed
        es = concat $ 
            concatMap (zipWith (\(a,ah) (b,bh) ->
                (if abs (ah - bh) <= 1
                then [(a,b),(b,a)]
                else if ah-bh <= 1
                then [(b,a)]
                else [(a,b) | bh-ah <=1]))
                <*> tail) (indexed1 ++ transpose indexed1)
    in show (
        evalState (dijkstra dst org es) (IM.singleton org (0, False))
        )


part2 :: String -> String
part2 s = 
    let (indexed, l) = flip runState 1 $ mapM (
            mapM (\a -> (,fromEnum a) <$> get <* modify (+1))
            ) $ lines s
        orgs = map fst $ filter (
            (\x ->x==83 || x == fromEnum 'a') . snd
            ) $ concat indexed
        dst = maybe 0 fst $ find ((==69) . snd) $ concat indexed
        indexed1 = map (map (\case
            (i,83) -> (i,fromEnum 'a')
            (i,69) -> (i,fromEnum 'z')
            x -> x)) indexed
        es = concat $ 
            concatMap (zipWith (\(a,ah) (b,bh) ->
                (if abs (ah - bh) <= 1
                then [(a,b),(b,a)]
                else if ah-bh <= 1
                then [(b,a)]
                else [(a,b) | bh-ah <=1]))
                <*> tail) (indexed1 ++ transpose indexed1)
    in show $ minimum $ map (\org -> 
        evalState (dijkstra dst org es) (IM.singleton org (0, False))
        ) orgs


dijkstra
    :: IM.Key
    -> IM.Key 
    -> [(Int,Int)] 
    -> State (IM.IntMap (Int, Bool)) Int
dijkstra dst i es
    | dst == i  = fst <$> gets (IM.findWithDefault (0, True) i)
    | otherwise = do
    (d, v) <- gets $ IM.findWithDefault (0, True) i
    modify (IM.adjust (second (const True)) i)
    let (es1, esr) = partition ((==i) . fst) es
        es2 = map snd es1
    mapM_ (\i1 -> do
        (d1, v1) <- gets $ IM.findWithDefault (maxBound, False) i1
        when (not v1 && d1 > (d + 1))
            (modify (IM.insert i1 (d+1, False)))
        ) es2
    next <- fst <$> gets (
        IM.foldrWithKey (\i1 (d1,v1) (pi,pd) -> 
            bool (pi,pd) (i1,d1) (not v1 && d1 < pd)
            ) (-1, maxBound))
    if next == -1 then return maxBound else
        dijkstra dst next esr
