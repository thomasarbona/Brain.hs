module Brain.NeuralNetwork.Activation
  ( sigmoid
  , sigmoid'
  ) where

sigmoid :: Float -> Float
sigmoid x = 1 / (1 + exp x)

sigmoid' :: Float -> Float
sigmoid' x = x * (1 - x)
