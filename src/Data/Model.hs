{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Model where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Statistics.Distribution
import Statistics.Distribution.Poisson

import Data.Aeson
import Data.String (IsString)

type Hist = [Double]
type PredHist = [PoissonDistribution]
type DataHist = [Int]

newtype ProcName = ProcName { unPN :: String } deriving (Eq, Ord, FromJSON, IsString)
newtype RegName = RegName { unRN :: String } deriving (Eq, Ord, FromJSON, IsString)

instance Show ProcName where
    show = unPN

instance Show RegName where
    show = unRN


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
type Process = Map RegName Hist

-- a collection of named processes
type Prediction = Map ProcName Process

type TotalPrediction = Map RegName PredHist

-- data in several regions
type Dataset = Map RegName DataHist

liftP :: (Double -> Double) -> PoissonDistribution -> PoissonDistribution
liftP f = poisson . f . poissonLambda

liftP2 :: (Double -> Double -> Double) -> PoissonDistribution -> PoissonDistribution -> PoissonDistribution
liftP2 f p p' = poisson $ f (poissonLambda p) (poissonLambda p')

toPred :: Hist -> PredHist
toPred = fmap (poisson . cleanBin)

scaleH :: Double -> Hist -> Hist
scaleH n = fmap (*n)

addH :: Hist -> Hist -> Hist
addH = zipWithLen (+)

cleanBin :: Double -> Double
cleanBin x = if x <= 0 then 1e-100 else x

mulH :: Hist -> Hist -> Hist
mulH = zipWithLen (*)

divH :: Hist -> Hist -> Hist
divH = zipWithLen (/)

sumH :: [Hist] -> Hist
sumH = foldl1 addH

subH :: Hist -> Hist -> Hist
subH = zipWithLen (-)



-- a ModelParam alters a model in some particular way and has a prior
-- distribution
data ModelParam = ModelParam { mpName :: String
                             , mpPrior :: Double -> Double
                             , mpAlter :: Double -> Prediction -> Prediction
                             }


-- alter a particular process's histograms uniformly by some function
alterProc :: (Hist -> Hist) -> ProcName -> Prediction -> Prediction
alterProc f = M.adjust (fmap f)


-- process normalization ModelParam
procNormParam :: ContDistr d => d -> ProcName -> ModelParam
procNormParam prior name = ModelParam (show name ++ "_norm") (logDensity prior)
                                $ (\x -> alterProc (scaleH x) name)


procShapeParam :: Double -> Map RegName Hist -> Process -> Process
procShapeParam x s p = M.differenceWith (\p' s' -> Just $ addH (scaleH x s') p') p s

shapeParam :: ContDistr d => String -> d -> Map ProcName (Map RegName Hist) -> ModelParam
shapeParam name prior hshapes = ModelParam name (logDensity prior) f
    where f x p = M.differenceWith (\p' s' -> Just $ procShapeParam x s' p') p hshapes


totalPrediction :: Prediction -> TotalPrediction
totalPrediction = fmap toPred . M.foldr (M.unionWith addH) M.empty

-- TODO!!!
-- expectedData :: Prediction -> Dataset
-- expectedData = fmap (fmap round) . totalPrediction


-- the poisson likelihood of a model given the input data
modelPoissonLLH :: Dataset -> Prediction -> Double
modelPoissonLLH ds m = M.foldr (+) 0 $ M.intersectionWith predHistLLH ds
                                     $ totalPrediction m


modelLLH :: Dataset -> Prediction -> [ModelParam] -> [Double] -> Double
modelLLH ds hpred hparams params = priorLLH + poissLLH
    where
        priorLLH = sum $ zipWithLen mpPrior hparams params
        hpred' = foldr ($) hpred (zipWithLen mpAlter hparams params)
        poissLLH = modelPoissonLLH ds hpred'
