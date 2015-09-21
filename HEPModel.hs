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

-- a process's normalization is consistent across regions
type Process = Map RegionName (Hist Double)

type Region = Map ProcessName (Hist Double)


data SystematicVariation =
    | Normalization Map RegionName Region -> 

type Dataset = Map RegionName (Hist Int)

-- HEPModel a takes a map of regions, a set of parameters
type HEPModel a = Map RegionName Region -> a -> Double

-- a ModelParameter alters a model in some particular way and has a prior
-- distribution in its parameter of type a
data ModelParameter a = ModelParameter {
    alter :: Model -> a -> Model,
    priorProb :: a -> Double
}


totalPrediction :: Model -> Process
totalPrediction = fmap (sumHists . M.elems) . regions


-- the poisson likelihood of a model given the input data
modelPoissonLH :: Dataset -> Model -> Double
modelPoissonLH ds m = product . M.elems $ M.intersectionWith poissonProbs (totalPrediction m) ds
