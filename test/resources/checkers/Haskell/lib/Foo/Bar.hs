module Foo.Bar where

hello :: String -> IO ()
hello s = putStrLn ("Hello " ++ s ++ "!")
