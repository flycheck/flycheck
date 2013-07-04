main :: IO ()
main = putStrLn . (\x -> unwords x) $ ["Hello"]
