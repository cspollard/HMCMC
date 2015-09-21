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


gaussianPdf :: (Double, Double) -> Double -> Double
gaussianPdf (mu, sigma) x = D.ugaussianPdf $ (x - mu) / sigma

gaussianProbs :: [(Double, Double)] -> [Double] -> Double
gaussianProbs musigmas = product . zipWith gaussianPdf musigmas

circleProb :: Double -> [Double] -> Double
circleProb radius xs = if (sum . map (\x -> x*x)) xs > radius then 0 else 1

poissonProbs :: [Double] -> [Int] -> Double
poissonProbs mus = product . zipWith poissonProb mus

poissonProb :: Double -> Int -> Double
poissonProb = flip D.poissonPdf


flatProb :: [(Double, Double)] -> [Double] -> Double
flatProb minmaxs = product . zipWith (\(mn, mx) x -> D.flatPdf x mn mx) minmaxs
