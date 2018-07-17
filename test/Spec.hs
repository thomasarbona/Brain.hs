import Test.Hspec
import Data.Matrix
import Brain.NeuralNetwork

main :: IO ()
main = hspec $ do
  describe "Brain.NeuralNetwork.network" $ do
    it "has correct weights layers" $ do
      (length weights' == 2) &&
        (nrows (weights' !! 0) == 2) &&
        (ncols (weights' !! 0) == 2) &&
        (nrows (weights' !! 1) == 1) &&
        (ncols (weights' !! 1) == 2)
    it "has correct bias layers" $ do
      (length bias' == 2) &&
        (nrows (bias' !! 0) == 2) &&
        (ncols (bias' !! 0) == 1) &&
        (nrows (bias' !! 1) == 1) &&
        (ncols (bias' !! 1) == 1)
    it "has correct nodes" $ do
      nodes' == nodes nn
    it "has correct learning rate" $ do
      learningRate nn == 0.1

  describe "Brain.NeuralNetwork.feed" $ do
    it "has credible output" $ do
      length out == 1
  where
    nn = network nodes'
    out = feed nn [0, 1]
    nodes' = [2, 2, 1]
    weights' = weights nn
    bias' = bias nn
