module Brain.NeuralNetwork.Network (
    NeuralNetwork(..)
  , NN
  , MatrixF
  , network
  , feed
  , feed2
  ) where

import Data.Matrix (
    Matrix
  , matrix
  , fromList
  , fromLists
  , toList
  , multStd2
  , elementwise
  , transpose
  , mapPos )
import Brain.NeuralNetwork.Activation (sigmoid, sigmoid')
import Brain.Helpers (toFixed, replaceNth, randomList)
import Data.List (zip5)
import Debug.Trace

data NeuralNetwork = NeuralNetwork {
  weights :: [MatrixF],
  bias :: [MatrixF],
  nodes :: [Int],
  learningRate :: Float
} deriving (Show)

type NN = NeuralNetwork
type MatrixF = Matrix Float

-- Create the neural network
-- Take an array representing
-- the number of nodes at each layer
network :: [Int] -> IO NN
network nodes = do
  randList <- randomList (-1) 1
  -- randList <- return(repeat 0.5)
  return $ network' nodes randList

-- Create the neural network
-- Take and array like network function but also an
-- array of float for generate the weights/bias with this values
network' :: [Int] -> [Float] -> NN
network' nodes numList = nn
  where
    nn = NeuralNetwork{
      weights = weights,
      bias = bias,
      nodes = nodes,
      learningRate = 0.1
    }
    nodes' = zip (init nodes) (tail nodes)
    datas = snd $ foldr fold (numList, []) nodes'
    weights = map (\((p, n), w, _) -> fromList n p w) datas
    bias = map (\((_, n), _, b) -> fromList n 1 b) datas
    fold (p, n) (r, a) = (drop (p * n) r, ((p, n), take (p * n) $ drop n r, take n r):a)

-- Feed forward the neural network
-- Take a NN and an array of input
feed :: NN -> [Float] -> [Float]
feed nn inputArr = toList output
  where
    input = fromList (length inputArr) 1 inputArr
    output = foldl feedLayer input $ zip (weights nn) (bias nn)

-- Feed forward the neural network
-- Take a NN and an array of input
-- But return a matrix array with the output of each layers
feed2 :: NN -> [Float] -> [MatrixF]
feed2 nn inputArr = outputs
  where
    input = fromLists $ foldr (\n a -> [n]:a) [] inputArr
    outputs = foldl fold [input] $ zip (weights nn) (bias nn)
    fold a n = a ++ [feedLayer (last a) n]

-- Feed forward only one layer of the neural network
-- Take the inputs and a pair of weights ant bias
-- Return the output of the layer
feedLayer :: MatrixF -> (MatrixF, MatrixF) -> MatrixF
feedLayer input (weights, bias) = mapPos (\_ n -> sigmoid n) b
  where
    a = multStd2 weights input
    b = elementwise (+) a bias
