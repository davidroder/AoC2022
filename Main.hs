{-# OPTIONS_GHC -F -pgmF discover/discover #-}

main :: IO ()
main = do
    (day:part:args) <- getArgs
    let dayL = map toLower day
    input <- case args of
        ("example":_) -> getContents
        [] -> readFile ("Input/" ++ dayL ++ ".txt")
    putStrLn $ case part of
        "1" -> part1 dayL input
        "2" -> part2 dayL input
        _   -> "Invalid part"