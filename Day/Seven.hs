module Day.Seven (part1, part2) where
import Text.ParserCombinators.ReadP
    ( (<++),
      char,
      eof,
      get,
      many,
      many1,
      manyTill,
      readP_to_S,
      satisfy,
      string,
      ReadP )
import Data.Char (isDigit)
import Data.Functor (void)
import Data.Monoid (Sum(Sum, getSum))


part1 :: String -> String
part1 = show . directorySizes . fst . head . readP_to_S (parseDirListing (Dir 0 "/" []))

directorySizes :: FileSystem -> Int
directorySizes (File _ _) = 0
directorySizes (Dir s _ xs)
    | s <= 100000 = s + sum (map directorySizes xs)
    | otherwise = sum (map directorySizes xs)

part2 :: String -> String
part2 = show . (minOverX =<< subtract 40000000 . size) . fst . head . readP_to_S (parseDirListing (Dir 0 "/" []))

minOverX :: Int -> FileSystem -> Int
minOverX _ (File _ _) = maxBound
minOverX x (Dir s _ xs)
    | s <= x = maxBound
    | otherwise = minimum (s:map (minOverX x) xs)

data FileSystem = File Int String | Dir Int String [FileSystem] deriving Show

size :: FileSystem -> Int
size (File s _) = s
size (Dir s _ _) = s

parseFileName :: ReadP FileSystem
parseFileName = do
    size <- read <$> many1 (satisfy isDigit)
    char ' '
    name <- manyTill get (char '\n')
    return $ File size name

parseDirName :: ReadP FileSystem
parseDirName = do
    string "dir "
    name <- manyTill get (char '\n')
    return $ Dir 0 name []

parseDirListing :: FileSystem -> ReadP FileSystem
parseDirListing (Dir _ name _) = do
    string "$ cd "
    string name
    string "\n$ ls\n"
    fs <- many (parseDirName <++ parseFileName)
    fs2 <- mapM parseDirListing fs
    let total = sum $ map size fs2
    void (string "$ cd ..\n") <++ eof
    return $ Dir total name fs2
parseDirListing f@(File _ _) = return f