module Main where

foo = 10 :: Integer

bar :: [String] -> [[String]]
bar xs = map lines xs

main :: IO ()
main = (putStrLn "Foobar")
