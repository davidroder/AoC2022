module Day.Eleven (part1, part2) where
import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence as S
import Data.Sequence (Seq, (|>))
import Text.ParserCombinators.ReadP (ReadP, string, many1, satisfy, char, skipSpaces, many, sepBy, (<++), readP_to_S, munch1, manyTill, eof)
import Data.Char (isDigit)
import Data.Bool (bool)
import Control.Monad.State.Lazy (runState, execState, get, put)
import Data.List (sortOn, foldl1')
import Data.Ord (Down(Down))


part1 :: String -> String
part1 s = 
    let monkeys = IM.fromList $ map snd $ fst $ head $ 
            readP_to_S (manyTill parseMonkey eof) s
    in  show . product . take 2 . sortOn Down . map itemsInspected 
         . IM.elems . flip execState monkeys $ mapM_ (\i -> do
        im <- get
        let (Monkey l o t n) = im IM.! i
            n1 = n + length l
            l1 = fmap (((,) =<< t) . (`div` 3) . o) l
            im1 = foldl (\it (k,v) -> IM.adjust (throwItem v) k it) im l1
            im2 = IM.insert i (Monkey S.empty o t n1) im1
        put im2) $ concat $ replicate 20 $ IM.keys monkeys



part2 :: String -> String
part2 s =
    let (ds, ms) = unzip $ fst $ head $ 
            readP_to_S (manyTill parseMonkey eof) s
        monkeys = IM.fromList ms
        divisor = foldl1' lcm ds
    in  show . product . take 2 . sortOn Down . map itemsInspected 
         . IM.elems . flip execState monkeys $ mapM_ (\i -> do
            im <- get
            let (Monkey l o t n) = im IM.! i
                n1 = n + length l
                l1 = fmap (((,) =<< t) . (`mod` divisor) . o) l
                im1 = foldl (\it (k,v) -> IM.adjust (throwItem v) k it) im l1
                im2 = IM.insert i (Monkey S.empty o t n1) im1
            put im2) $ concat $ replicate 10000 $ IM.keys monkeys

data Monkey = Monkey {
    items :: Seq Int, 
    operation :: Int -> Int,
    throwTo :: Int -> Int,
    itemsInspected :: Int
}
parseMonkey :: ReadP (Int, (Int, Monkey))
parseMonkey = do
    string "Monkey "
    n <- parseInt
    char ':'
    skipSpaces
    string "Starting items:"
    items <- (skipSpaces *> parseInt) `sepBy` char ','
    skipSpaces
    string "Operation: new = "
    o1 <- parseOperand
    skipSpaces
    op <- parseOperator
    skipSpaces
    o2 <- parseOperand
    skipSpaces
    string "Test: divisible by "
    db <- parseInt
    skipSpaces
    string "If true: throw to monkey "
    t <- parseInt
    skipSpaces
    string "If false: throw to monkey "
    f <- parseInt
    skipSpaces
    return (db, (n, 
        Monkey 
            (S.fromList items)
            (op <$> o1 <*> o2)
            (bool f t . (==0) . (`mod` db))
            0))

throwItem :: Int -> Monkey -> Monkey
throwItem i (Monkey l o t n) = Monkey (l |> i) o t n

parseInt :: (Integral a, Read a) => ReadP a
parseInt = read <$> munch1 isDigit

parseOperand :: (Integral a, Read a) => ReadP (a -> a)
parseOperand = (const <$> parseInt) <++ (id <$ string "old")

parseOperator :: (Integral a, Read a) => ReadP (a -> a -> a)
parseOperator = 
    ((*) <$ char '*') <++
    ((+) <$ char '+') <++
    ((-) <$ char '-') <++
    (div <$ char '/')