module Brain.NeuralNetwork.Training
  ( train
  , learn
  ) where

import Brain.NeuralNetwork.Network (NeuralNetwork(..), NN, feed2, MatrixF)
import Data.Matrix (
    fromList
  , elementwise
  , transpose
  , multStd2
  , mapPos )
import Brain.NeuralNetwork.Activation (sigmoid, sigmoid')
import Brain.Helpers (toFixed, replaceNth, randomList)
import Data.List (zip5)
import Debug.Trace

-- Train the neureal network
-- with an array of training data
train :: NN -> [([Float], [Float])] -> NN
train nn training = foldl learn nn training

-- Train the neural network with one input/target
-- Take the neural network, and a pair representing
-- the input and the target
learn :: NN -> ([Float], [Float]) -> NN
learn nn (inputArr, targetArr) = foldr fold nn datas
  where
    target = fromList 1 (length targetArr) targetArr
    outputs' = feed2 nn inputArr
    outputs = tail $ zip outputs' $ 0:init outputs'
    errors = computeErrors nn target outputs'
    datas = zip5 (weights nn) (bias nn) errors outputs [0..]
    fold (w, b, e, o, i) a = learn' a (w, b) e o i
    learn' nn (w, b) errors (output, prevOutput) index = nn'
      where
        gradients'' = mapPos (\_ n -> sigmoid' n) output
        gradients' = elementwise (*) gradients'' errors
        gradients = mapPos (\_ n -> n * (learningRate nn)) gradients'
        deltas = multStd2 gradients $ transpose prevOutput
        w' = replaceNth index (elementwise (+) w deltas) $ weights nn
        b' = replaceNth index (elementwise (+) b gradients) $ bias nn
        nn' = nn{ weights = w', bias = b' }

-- Compute the errors of each layers of the neural network
-- Take the neural network, the target and the output of each layers
-- Return an array of matrix representing the errors of each layers
computeErrors :: NN -> MatrixF -> [MatrixF] -> [MatrixF]
computeErrors nn target outputs = foldr fold [outerr] (tail $ weights nn)
  where
    fold w a = (computeErrors' w $ head a) : a
    outerr = elementwise (-) target (last outputs)
    computeErrors' :: MatrixF -> MatrixF -> MatrixF
    computeErrors' w prevLayErr = multStd2 (transpose w) prevLayErr
