module HEPModel where

import Data.Map (Map)
import qualified Data.Map as M
import Data.HMCMC.Dist


type ProcessName = String
type RegionName = String

-- simple histogram type
type Hist a = [a]

sumHists :: Num a => [Hist a] -> Hist a
sumHists = foldr1 (zipWith (+))

-- a sample's normalization is consistent across regions
type Process = Map RegionName (Hist Double)

type Region = Map ProcessName (Hist Double)

-- a variation alters a model in some particular way.
type Variation = Double -> Model -> Model

type Model = Map RegionName Region

type Dataset = Map RegionName (Hist Int)

totalPrediction :: Model -> Process
totalPrediction = fmap (sumHists . M.elems)

-- the poisson likelihood of a model given the input data
modelPoissonLH :: Dataset -> Model -> Double
modelPoissonLH ds m = product $ zipWith poissonProbs (M.elems (totalPrediction m)) (M.elems ds)
