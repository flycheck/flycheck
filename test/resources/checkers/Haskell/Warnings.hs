module Haskell.Warnings
where

import Haskell.Internal (bar)

foo = 10 :: Integer

spam :: [String] -> [[String]]
spam eggs = map lines eggs

main :: IO ()
main = (putStrLn bar)
