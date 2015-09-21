module Main where

import System.Random

import Data.HMCMC
import Data.HMCMC.Dist

import HEPModel
import Data.Map (singleton, fromList)

dataSet = singleton "SR" [10, 11, 9, 7, 6, 4, 1, 0, 1, 0]

predMC = fromList [("MC1", singleton "SR" $ map (*0.5) [7.5, 7.3, 6.0, 4.3, 3.5, 0.9, 0.7, 0.4, 0.1, 0.05]),
    ("MC2", singleton "SR" [3.1, 3.3, 3.2, 2.6, 2.4, 1.9, 1.6, 0.9, 0.3, 0.1])]

mc1Norm = procNormParam (\x -> if x < 0 then 0.0 else gaussianPdf (0.3, 1.0) x) "MC1"
mc2Norm = procNormParam (\x -> if x < 0 then 0.0 else gaussianPdf (0.3, 1.0) x) "MC2"



gaussProps :: RandomGen g => [Double] -> ([Double], g) -> ([Double], g)
gaussProps [] ([], gen) = ([], gen)
gaussProps (mu:mus) (x:xs, gen) =
    let (x', gen'') = sampleGaussian ((x, mu), gen') in (x':xs', gen'')
    where
        (xs', gen') = gaussProps mus (xs, gen) 
gaussProps _ _ = undefined


appMany :: [a -> b] -> a -> [b]
appMany fs x = ($ x) `fmap` fs

main :: IO ()
-- main = print . appMany [mean, mode, median, stddev] . map fst . take 999999 $ iterateS testState ((1, 1), mkStdGen 0)
main = mapM_ (\xs -> mapM_ (\x -> putStr (show x) >> putChar ' ') xs >> putChar '\n') . take 999999 $ iterateS testState ([1, 1], mkStdGen 0)
    where
        testState :: RandomGen g => RandomWalk g [Double]
        testState = mcmc (gaussProps [0.05, 0.05])
                        (modelLH dataSet predMC [mc1Norm, mc2Norm])
