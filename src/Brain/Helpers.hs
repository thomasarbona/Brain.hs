module Brain.Helpers
  ( toFixed
  , replaceNth
  , randomList
  , shuffle
  ) where

import System.Random (randomRs, getStdGen, randomRIO)

toFixed :: Float -> Int -> Float
toFixed n x =  (fromInteger $ round $ n * (10 ^ x)) / (10.0 ^^ x)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
 | n == 0 = newVal:xs
 | otherwise = x:replaceNth (n-1) newVal xs

randomList :: Float -> Float -> IO [Float]
randomList min max =
  getStdGen >>= return . randomRs (min, max)

shuffle :: [a] -> IO [a]
shuffle x = if length x < 2 then return x else do
  i <- System.Random.randomRIO (0, length(x)-1)
  r <- shuffle (take i x ++ drop (i+1) x)
  return (x!!i : r)
