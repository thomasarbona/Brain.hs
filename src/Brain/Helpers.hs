module Brain.Helpers
  ( toFixed
  ) where

toFixed :: Float -> Int -> Float
toFixed n x =  (fromInteger $ round $ n * (10 ^ x)) / (10.0 ^^ x)
