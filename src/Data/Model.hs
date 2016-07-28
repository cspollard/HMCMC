{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Model where

import Data.Map (Map)
import qualified Data.Map as M

import Statistics.Distribution
import Statistics.Distribution.Poisson
import Control.Monad.Primitive (PrimMonad)

type SFHist = [Double]
type PredHist = [PoissonDistribution]
type DataHist = [Int]

rebinH :: Num a => Int -> [a] -> [a]
rebinH _ []            = []
rebinH n h | n <= 1    = h
           | otherwise = let (hs, hs') = splitAt n h
                         in  sum hs : rebinH n hs'

zipWithLen :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithLen f (a:as) (b:bs) = f a b : zipWithLen f as bs
zipWithLen _ [] []         = []
zipWithLen _ _ _           = error "zipping lists of different lengths"

-- the log likelihood of a prediction histogram given a data histogram
predHistLLH :: DataHist -> PredHist -> Double
predHistLLH dh ph = sum $ zipWithLen logProbability ph dh


-- a process's normalization is consistent across regions
type Process = Map String PredHist

-- a collection of named processes
type Prediction = Map String Process

-- data in several regions
type Dataset = Map String DataHist

liftP :: (Double -> Double) -> PoissonDistribution -> PoissonDistribution
liftP f = poisson . f . poissonLambda

liftP2 :: (Double -> Double -> Double) -> PoissonDistribution -> PoissonDistribution -> PoissonDistribution
liftP2 f p p' = poisson $ f (poissonLambda p) (poissonLambda p')

scaleH :: Double -> PredHist -> PredHist
scaleH n = fmap (liftP (*n))

addH :: PredHist -> PredHist -> PredHist
addH = zipWithLen (liftP2 (+))

mulH :: SFHist -> PredHist -> PredHist
mulH sfh = fmap poisson . zipWithLen (*) sfh . fmap poissonLambda

sumH :: [PredHist] -> PredHist
sumH = foldl1 addH




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


procShapeParam :: ContDistr d => SFHist -> d -> String -> ModelParam
procShapeParam hshape prior name = ModelParam (logDensity prior) $
                                    (\x -> alterProc (mulH (fmap (*x) hshape)) name)


totalPrediction :: Prediction -> Process
totalPrediction = M.foldr (M.unionWith addH) M.empty


-- TODO!!!
-- expectedData :: Prediction -> Dataset
-- expectedData = fmap (fmap round) . totalPrediction


-- the poisson likelihood of a model given the input data
modelPoissonLLH :: Dataset -> Prediction -> Double
modelPoissonLLH ds m = M.foldr (+) 0 $ M.intersectionWith predHistLLH ds (totalPrediction m)


modelLLH :: Dataset -> Prediction -> [ModelParam] -> [Double] -> Double
modelLLH ds hpred hparams params = priorLLH + poissLLH
    where
        priorLLH = sum $ zipWithLen mpPrior hparams params
        hpred' = foldr ($) hpred (zipWithLen mpAlter hparams params)
        poissLLH = modelPoissonLLH ds hpred'
