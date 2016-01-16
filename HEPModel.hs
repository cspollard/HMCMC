module HEPModel where

import Data.Map (Map)
import qualified Data.Map as M
import Data.HMCMC.Dist (poissonLogProbs)

import Numeric.GSL.Minimization
import Numeric.LinearAlgebra.Data (Matrix)

import Debug.Trace

-- TODO
-- IntMaps should be much faster for totalPrediction

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
    hmpRange :: (Double, Double),
    hmpPriorLogProb :: Double -> Double,
    hmpAlter :: Double -> HEPPrediction -> HEPPrediction
}


type HEPWorkSpace = (Dataset, HEPPrediction, [HEPModelParam])


-- alter a particular process's histograms uniformly by some function
alterProc :: (Hist Double -> Hist Double) -> String -> HEPPrediction -> HEPPrediction
alterProc f = M.adjust (fmap f)


-- process normalization HEPModelParam
procNormParam :: Double -> (Double -> Double) -> String -> HEPModelParam
procNormParam xmax prior name = HEPModelParam (0, xmax) prior (\x -> alterProc (scaleH x) name)


procShapeParam :: Hist Double -> (Double, Double) -> (Double -> Double) -> String -> HEPModelParam
procShapeParam hshape range prior name = HEPModelParam range prior (\x -> alterProc (mulH (scaleH x hshape)) name)


totalPrediction :: HEPPrediction -> Process
totalPrediction = M.foldr (M.unionWith addH) M.empty


expectedData :: HEPPrediction -> Dataset
expectedData = fmap (fmap round) . totalPrediction


-- the poisson likelihood of a model given the input data
modelPoissonLogProb :: Dataset -> HEPPrediction -> Double
modelPoissonLogProb ds m = M.foldr (+) 0 $ M.intersectionWith poissonLogProbs (totalPrediction m) ds

modelLogProb :: HEPWorkSpace -> [Double] -> Double
modelLogProb (ds, hpred, hparams) params =
            -- guard against infinities
            if isInfinite lp
                then (-1e100)
                else if isNaN lp
                    then traceShow params lp
                    else lp
            where
                priorLogProb = sum $ zipWith hmpPriorLogProb hparams params
                hpred' = foldr ($) hpred (zipWith hmpAlter hparams params)
                poissLogProb = modelPoissonLogProb ds hpred'
                lp = priorLogProb + poissLogProb


bestFit :: HEPWorkSpace -> [Double] -> ([Double], Matrix Double)
bestFit hws@(_, _, hparams) startParams =
        minimize NMSimplex2 10 9999 boxes (negate . modelLogProb hws) startParams
    where
        searchBox hp x = let (xmin, xmax) = hmpRange hp in max (abs (x - xmin)) (abs (xmax - x))
        boxes = traceShowId $ zipWith searchBox hparams startParams
