module HEPModel where

import Data.Map (Map)
import qualified Data.Map as M
import Data.HMCMC.Dist (poissonProbs)

-- simple histogram type
type Hist a = [a]

scaleH :: Num a => a -> Hist a -> Hist a
scaleH n = map (*n)

addH :: Num a => Hist a -> Hist a -> Hist a
addH = zipWith (+)

mulH :: Num a => Hist a -> Hist a -> Hist a
mulH = zipWith (*)

sumH :: Num a => [Hist a] -> Hist a
sumH = foldr1 addH


-- a process's normalization is consistent across regions
type Process = Map String (Hist Double)

-- a collection of named processes
type HEPPrediction = Map String Process

-- data in several regions
type Dataset = Map String (Hist Int)


-- a HEPModelParam alters a model in some particular way and has a prior
-- distribution in its parameter of type a
data HEPModelParam = HEPModelParam {
    hnpPriorProb :: Double -> Double,
    hnpAlter :: Double -> HEPPrediction -> HEPPrediction
}


-- alter a particular process's histograms uniformly by some function
alterProc :: (Hist Double -> Hist Double) -> String -> HEPPrediction -> HEPPrediction
alterProc f = M.adjust (fmap f)

-- process normalization HEPModelParam
procNormParam :: (Double -> Double) -> String -> HEPModelParam
procNormParam prior name = HEPModelParam prior (\x -> alterProc (scaleH x) name)

procShapeParam :: Hist Double -> (Double -> Double) -> String -> HEPModelParam
procShapeParam hshape prior name = HEPModelParam prior (\x -> alterProc (mulH (scaleH x hshape)) name)


totalPrediction :: HEPPrediction -> Process
totalPrediction = foldr1 (M.intersectionWith addH) . M.elems


expectedData :: HEPPrediction -> Dataset
expectedData = fmap (fmap round) . totalPrediction


-- the poisson likelihood of a model given the input data
modelPoissonLH :: Dataset -> HEPPrediction -> Double
modelPoissonLH ds m = product . M.elems $ M.intersectionWith poissonProbs (totalPrediction m) ds


modelLH :: Dataset -> HEPPrediction -> [HEPModelParam] -> [Double] -> Double
modelLH ds hpred hparams params = priorLH * poissLH
    where
        priorLH = product $ zipWith hnpPriorProb hparams params
        hpred' = foldr ($) hpred (zipWith hnpAlter hparams params)
        poissLH = modelPoissonLH ds hpred'


