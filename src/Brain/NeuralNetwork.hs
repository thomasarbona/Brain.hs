module Brain.NeuralNetwork (
    NeuralNetwork(..)
  , network
  , feed
  ) where

import Data.Matrix (
    Matrix
  , matrix
  , fromLists
  , toList
  , multStd2
  , elementwise
  , elementwiseUnsafe
  , mapPos )
import Brain.NeuralNetwork.Activation (sigmoid, sigmoid')
import Brain.Helpers (toFixed)
import Debug.Trace

data NeuralNetwork = NeuralNetwork {
  weights :: [Matrix Float],
  bias :: [Matrix Float],
  nodes :: [Int],
  learningRate :: Float
} deriving (Show)

type NN = NeuralNetwork

-- Create the neural network
-- Take an array representing
-- the number of nodes at each layer
network :: [Int] -> NN
network nodes = nn
  where
    nn = NeuralNetwork{
      weights = weights,
      bias = bias,
      nodes = nodes,
      learningRate = 0.1
    }
    weights = map (\(p, n) -> matrix n p gen) $ zip (init nodes) (tail nodes)
    bias = map (\n -> matrix n 1 gen) $ tail nodes
    gen (a, b) = toFixed (cos $ realToFrac $ a * b) 2

-- Feed forward the neural network
-- Take a NN and an array of input
feed :: NN -> [Float] -> [Float]
feed nn inputArr = toList output
  where
    input = fromLists $ foldr (\n a -> [n]:a) [] inputArr
    output = foldl feedLayer input $ zip (weights nn) (bias nn)
    feedLayer input (weights, bias) = mapPos (\_ n -> sigmoid n) b
      where
        a = multStd2 weights input
        b = elementwiseUnsafe (+) a bias
