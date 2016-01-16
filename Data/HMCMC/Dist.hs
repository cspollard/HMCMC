module Data.HMCMC.Dist where

import qualified GSL.Random.Dist as D
import System.Random (RandomGen, Random(..))

type SampleDist g a = (a, g) -> (Double, g)

sampleUniform :: RandomGen g => SampleDist g (Double, Double)
sampleUniform ((mn, mx), gen) = (x', gen')
    where
        (x, gen') = random gen
        x' = (x * (mx - mn)) + mn

sampleGaussian :: RandomGen g => SampleDist g (Double, Double)
sampleGaussian = sample gaussianCDFInv


sample :: RandomGen g => (a -> Double -> Double) -> SampleDist g a
sample cdfInv (params, gen) = (x', gen')
    where 
        (x, gen') = sampleUniform ((0, 1), gen)
        x' = cdfInv params x


gaussianCDFInv :: (Double, Double) -> Double -> Double
gaussianCDFInv (mu, sigma) x = D.gaussianPInv x 1.0 * sigma + mu


sampleForever :: RandomGen g => SampleDist g a -> (a, g) -> [Double]
sampleForever f (a, g) = let (b, g') = f (a, g) in b : sampleForever f (a, g')


gaussianProb :: (Double, Double) -> Double -> Double
gaussianProb (mu, sigma) x = D.ugaussianPdf $ (x - mu) / sigma

gaussianLogProb :: (Double, Double) -> Double -> Double
gaussianLogProb (mu, sigma) = log . gaussianProb (mu, sigma)

gaussianProbs :: [(Double, Double)] -> [Double] -> Double
gaussianProbs musigmas = product . zipWith gaussianProb musigmas

gaussianLogProbs :: [(Double, Double)] -> [Double] -> Double
gaussianLogProbs musigmas = sum . zipWith gaussianLogProb musigmas

circleProb :: Double -> [Double] -> Double
circleProb radius xs = if (sum . map (\x -> x*x)) xs > radius then 0 else 1

poissonProb :: Double -> Int -> Double
poissonProb = flip D.poissonPdf

poissonLogProb :: Double -> Int -> Double
poissonLogProb l = log . poissonProb l

poissonProbs :: [Double] -> [Int] -> Double
poissonProbs ls = product . zipWith poissonProb ls

poissonLogProbs :: [Double] -> [Int] -> Double
poissonLogProbs ls = sum . zipWith poissonLogProb ls


flatProb :: [(Double, Double)] -> [Double] -> Double
flatProb minmaxs = product . zipWith (\(mn, mx) x -> D.flatPdf x mn mx) minmaxs
