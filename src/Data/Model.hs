{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Model where

import Data.MonoTraversable

import Data.Map (Map)
import qualified Data.Map as M

import Data.Vector

import Statistics.Distribution
import Statistics.Distribution.Poisson
import Control.Monad.Primitive (PrimMonad)


newtype PredHist = PredHist { phVec :: Vector PoissonDistribution } deriving MonoTraversable
newtype DataHist = DataHist { dhVec :: Vector Int } deriving MonoTraversable

-- a process's normalization is consistent across regions
type Process = Map String PredHist

-- a collection of named processes
type Prediction = Map String Process

-- data in several regions
type Dataset = Map String DataHist


-- the log likelihood of a prediction histogram given a data histogram
predHistLLH :: DataHist -> PredHist -> Double
predHistLLH dh ph = osum $ zipWith logProbability (otoList ph) (otoList dh)


scaleH :: Double -> PredHist -> PredHist
scaleH n = omap (*n)

addH :: PredHist -> PredHist -> PredHist
addH h h' = zipWith (+) (otoList h'

mulH :: PredHist -> PredHist -> PredHist
mulH = zipWith (*)

sumH :: [PredHist] -> PredHist
sumH = foldr1 addH




-- a ModelParam alters a model in some particular way and has a prior
-- distribution
data ModelParam = ModelParam {
    mpPrior :: Double -> Double,
    mpAlter :: Double -> Prediction -> Prediction
}


-- alter a particular process's histograms uniformly by some function
alterProc :: (PredHist -> PredHist) -> String -> Prediction -> Prediction
alterProc f = M.adjust (fmap f)


-- process normalization ModelParam
procNormParam :: ContDistr d => d -> String -> ModelParam
procNormParam prior name = ModelParam (logDensity prior) (\x -> alterProc (scaleH x) name)


procShapeParam :: ContDistr d => PredHist -> d -> String -> ModelParam
procShapeParam hshape prior name = ModelParam (logDensity prior) $
                                    (\x -> alterProc (mulH (scaleH x hshape)) name)


totalPrediction :: Prediction -> Process
totalPrediction = M.foldr (M.unionWith addH) M.empty


-- TODO!!!
expectedData :: Prediction -> Dataset
expectedData = fmap (fmap round) . totalPrediction


-- the poisson likelihood of a model given the input data
modelPoissonLLH :: Dataset -> Prediction -> Double
modelPoissonLLH ds m = M.foldr (+) 0 $ M.intersectionWith poissHistLLH ds (fmap (fmap poisson) $ totalPrediction m)



modelLLH :: Dataset -> Prediction -> [ModelParam] -> [Double] -> Double
modelLLH ds hpred hparams params = priorLLH + poissLLH
    where
        priorLLH = sum $ zipWith mpPrior hparams params
        hpred' = foldr ($) hpred (zipWith mpAlter hparams params)
        poissLLH = modelPoissonLLH ds hpred'
