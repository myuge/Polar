module Main (main) where

import Polar (projectName)


main :: IO ()
main = putStrLn ("Benchmarks for " ++ projectName)
