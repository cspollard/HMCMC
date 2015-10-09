module HEPModel where

import Data.Map (Map)
import qualified Data.Map as M
import Data.HMCMC.Dist (poissonProbs)

import Numeric.GSL.Minimization

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
    hmpPriorProb :: Double -> Double,
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
modelPoissonProb :: Dataset -> HEPPrediction -> Double
modelPoissonProb ds m = M.foldr (*) 1 $ M.intersectionWith poissonProbs (totalPrediction m) ds


modelProb :: HEPWorkSpace -> [Double] -> Double
modelProb (ds, hpred, hparams) params = priorProb * poissProb
    where
        priorProb = product $ zipWith hmpPriorProb hparams params
        hpred' = foldr ($) hpred (zipWith hmpAlter hparams params)
        poissProb = modelPoissonProb ds hpred'


bestFit :: HEPWorkSpace -> [Double] -> [Double]
bestFit hws@(_, _, hparams) startParams =
        fst $ minimize NMSimplex2 1e-10 9999 boxes (negate . modelProb hws) startParams
    where
        searchBox hp x = let (xmin, xmax) = hmpRange hp in max (abs (x - xmin)) (abs (xmax - x))
        boxes = zipWith searchBox hparams startParams
