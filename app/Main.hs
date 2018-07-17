module Main where

import Brain.NeuralNetwork

main :: IO ()
main = do
  print $ show (length w)
  where
    nn = network [2, 2, 1]
    w = weights nn
