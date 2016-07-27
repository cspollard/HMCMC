{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Model where

import Statistics.Distribution
import Statistics.Distribution.Poisson
import Control.Monad.Primitive (PrimMonad)

import Data.FixedList
import Data.Foldable

import Control.Lens.Type
import Control.Lens.At
import Control.Lens.Indexed

type PredHist f = Cons f PoissonDistribution
type DataHist f = Cons f Int

type instance Index (Cons f a) = Int
type instance Index (Nil a) = Int
type instance IxValue (Cons f a) = a 
type instance IxValue (Nil a) = a 

instance (Ixed (f a), Index (f a) ~ Int, IxValue (f a) ~ a, FixedList f) => Ixed (Cons f a) where
    ix k f xs0@(x :. xs) | k < 0     = pure xs0
                         | k == 0    = (:. xs) <$> f x
                         | otherwise = (x :.) <$> ix (k-1) f xs

instance Ixed (Nil a) where
    ix _ _ _ = pure Nil


instance (FixedList f, FoldableWithIndex Int f) => FoldableWithIndex Int (Cons f) where
    ifoldMap g (x :. xs) = g 0 x `mappend` ifoldMap (g . (+1)) xs

instance FoldableWithIndex Int Nil where
    ifoldMap _ _ = mempty


-- the log likelihood of a prediction histogram given a data histogram
predHistLLH :: FixedList f => DataHist f -> PredHist f -> Double
predHistLLH dh ph = sum $ logProbability <$> ph <*> dh

{-
-- a process's normalization is consistent across regions
type Process = Map String PredHist

-- a collection of named processes
type Prediction = Map String Process

-- data in several regions
type Dataset = Map String DataHist




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
-}
