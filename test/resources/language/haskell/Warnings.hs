module Haskell.Warnings (spam, main)
where

spam eggs = map lines eggs

main :: IO ()
main = (putStrLn "hello world")
