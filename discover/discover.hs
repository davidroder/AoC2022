module Main (main) where
import System.Directory (getDirectoryContents)
import Util (stripSuffix)
import Data.Maybe (mapMaybe)
import Data.Char (toLower)
import System.Environment (getArgs)
import Control.Monad.Writer (tell, execWriterT, lift)



main :: IO ()
main = do
    (orig:inp:out:_) <- getArgs
    fs <- mapMaybe (stripSuffix ".hs") <$> getDirectoryContents "Day"
    a <- execWriterT $ do
        tell "import System.Environment (getArgs)\n"
        tell "import Data.Char (toLower)\n"
        mapM_ (\f -> tell ("import qualified Day." ++ f ++ "\n")) fs
        tell "part1 :: String -> String -> String\n"
        mapM_ (\f -> tell ("part1 \"" ++ map toLower f ++ "\" = Day." ++ f ++ ".part1\n")) fs
        mapM_ (\f -> tell ("part2 \"" ++ map toLower f ++ "\" = Day." ++ f ++ ".part2\n")) fs
        tell =<< lift (readFile inp)
    writeFile out a
    