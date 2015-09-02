module Main where

import System.Random

import Data.HMCMC
import Data.HMCMC.Dist
import Data.HMCMC.Statistics

dataHist = [10, 11, 9, 7, 6, 4, 1, 0, 1, 0]

mcHist1 = map (*0.75) [7.5, 7.3, 6.0, 4.3, 3.5, 0.9, 0.7, 0.4, 0.1, 0.05]
mcHist2 = [3.1, 3.3, 3.2, 2.6, 2.4, 1.9, 1.6, 0.9, 0.3, 0.1]

mcNormProps ((x, y), gen) = ((x', y'), gen'')
    where
        (x', gen') = sampleGaussian ((x, 0.05), gen)
        (y', gen'') = sampleGaussian ((y, 0.05), gen')

tempProb :: (Double, Double) -> Double
tempProb (x, y) = poisLH * normLH
    where
        mcHist1Normed = map (*x) mcHist1
        mcHist2Normed = map (*y) mcHist2

        -- the poisson likelihood of the normed mc bkgs given the data
        -- histogram
        poisLH = poissonProb (zipWith (+) mcHist1Normed mcHist2Normed) dataHist

        -- the probability of the normalization give the mc priors
        -- normLH = gaussianProb [(1, 0.2), (1, 0.1)] [x, y]
        normLH = flatProb [(0, 3), (0, 2)] [x, y]


gaussProp :: RandomGen g => [Double] -> ([Double], g) -> ([Double], g)
gaussProp [] ([], gen) = ([], gen)
gaussProp (mu:mus) (x:xs, gen) =
    let (x', gen'') = sampleGaussian ((x, mu), gen') in (x':xs', gen'')
    where
        (xs', gen') = gaussProp mus (xs, gen) 
gaussProm _ _ = undefined


appMany :: [a -> b] -> a -> [b]
appMany fs x = ($ x) `fmap` fs

main :: IO ()
-- main = print . appMany [mean, mode, median, stddev] . map fst . take 999999 $ iterateS testState ((1, 1), mkStdGen 0)
main = mapM_ (print . snd) . take 999999 $ iterateS testState ((1, 1), mkStdGen 0)
    where
        testState :: RandomGen g => RandomWalk g (Double, Double)
        testState = mcmc mcNormProps tempProb
