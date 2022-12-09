module Day.Nine (part1, part2) where
import qualified Data.Set as S
import Control.Monad.State.Lazy
    ( MonadState(put, get), foldM, evalState, modify, runState )


part1 :: String -> String
part1 = show . S.size . flip evalState ((0,0),(0,0)) . foldM (\s ins -> do
    (h,t) <- get
    let h2 = appInst ins h
        t2 = follow h2 t
    put (h2, t2)
    return $ S.insert t2 s
    ) S.empty . concatMap (\(c:' ':n) -> replicate (read n) c) . lines

part2 :: String -> String
part2 = show . S.size . flip evalState ((0,0), replicate 9 (0,0)) . 
    foldM (\s ins -> do
        (h,ks) <- get
        let h2 = appInst ins h
            (ks2, t) = runState (mapM (\k -> do
                modify (`follow` k)
                get
                ) ks) h2
        put (h2, ks2)
        return $ S.insert t s
    ) S.empty . concatMap (\(c:' ':n) -> replicate (read n) c) . lines

appInst :: Char -> (Int,Int) -> (Int,Int)
appInst 'U' (x,y) = (x, y+1)
appInst 'D' (x,y) = (x, y-1)
appInst 'R' (x,y) = (x+1, y)
appInst 'L' (x,y) = (x-1, y)

follow :: (Int,Int) -> (Int,Int) -> (Int,Int)
follow (hx, hy) (tx, ty)
    | abs (hx - tx) <= 1
    , abs (hy - ty) <= 1
    = (tx, ty)
    | otherwise
    = ( tx + fromEnum (compare hx tx) - 1,
        ty + fromEnum (compare hy ty) - 1)

-- Nicer but seems to be slower:
-- follow (hx, hy) (tx, ty) = case (hx-tx,hy-ty) of
--     (dx, dy)
--         | abs dx <= 1
--         , abs dy <= 1
--         -> (tx, ty)
--         | otherwise
--         -> (tx + signum dx, ty + signum dy)