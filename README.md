# Brain

Basic neural network deep learning in Haskell. I made this to experiment neural network.

## Usage

Example for train a XOR neural network:
```Haskell
module Main where

import Brain.NeuralNetwork
import Brain.Helpers (shuffle)

training :: [([Float], [Float])]
training = [([0.0, 1.0], [1.0]),([1.0, 0.0], [1.0]),([1.0, 1.0], [0.0]),([0.0, 0.0], [0.0])]

main :: IO ()
main = do
  nn <- network [2, 4, 1]
  t <- shuffle $ take 10000 $ cycle training
  nn' <- return(train nn t)
  print $ "[1, 1]: " ++ (show $ feed nn' [1, 1])
  print $ "[0, 0]: " ++ (show $ feed nn' [0, 0])
  print $ "[0, 1]: " ++ (show $ feed nn' [0, 1])
  print $ "[1, 0]: " ++ (show $ feed nn' [1, 0])
```

Output:
```
"[1, 1]: [0.21777487]"
"[0, 0]: [0.11764489]"
"[0, 1]: [0.88554364]"
"[1, 0]: [0.83594877]"
```
