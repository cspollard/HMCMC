module Main where

import GSL.Random.Dist
import System.Random (RandomGen, Random(..), mkStdGen)
import Control.Monad.Trans.State.Lazy


type Sample g a = (a, g) -> (Double, g)

-- a RandomWalk computes the next state based on the current state and
-- the state of the random number generator.
type RandomWalk g a = State (a, g) a

sampleUniform :: RandomGen g => Sample g (Double, Double)
sampleUniform ((mn, mx), gen) = (x', gen')
    where
        (x, gen') = random gen
        x' = (x * (mx - mn)) + mn

sampleGaussian :: RandomGen g => Sample g (Double, Double)
sampleGaussian = sampleCDFInv gaussianCDFInv


sampleCDFInv :: RandomGen g => (a -> Double -> Double) -> Sample g a
sampleCDFInv cdfInv (params, gen) = (x', gen')
    where 
        (x, gen') = sampleUniform ((0, 1), gen)
        x' = cdfInv params x


gaussianCDFInv :: (Double, Double) -> Double -> Double
gaussianCDFInv (mu, sigma) x = gaussianPInv x 1.0 * sigma + mu


sampleForever :: RandomGen g => Sample g a -> (a, g) -> [Double]
sampleForever f (a, g) = let (b, g') = f (a, g) in b : sampleForever f (a, g')


gaussianPdfMuSigma :: Double -> Double -> Double -> Double
gaussianPdfMuSigma mu sigma x = ugaussianPdf $ (x - mu) / sigma


-- infinite iteration of Stateful computations
iterateS :: State s a -> s -> [a]
iterateS st s = let (v, s') = runState st s in v : iterateS st s'


runMCMC :: RandomGen g => ((a, g) -> (a, g)) -> (a -> Double) -> (a, g) -> (a, (a, g))
runMCMC propose prob (ps, gen) =
             if probPropose / probCurr > u
                then (ps', (ps', gen''))
                else (ps, (ps, gen''))
            where
                (ps', gen') = propose (ps, gen) 
                (u, gen'') = sampleUniform ((0, 1), gen')
                probCurr = prob ps
                probPropose = prob ps'


main :: IO ()
main = mapM_ (print . head) . take 999999 $ iterateS (testState [1]) ([0], mkStdGen 0)
    where

        guassianProb :: [(Double, Double)] -> [Double] -> Double
        guassianProb musigmas ps = product $ zipWith (\x (m, s) -> gaussianPdfMuSigma m s x) ps musigmas

        circleProb :: Double -> [Double] -> Double
        circleProb radius xs = if (sum . map (\x -> x*x)) xs > radius then 0 else 1

        -- TODO
        -- all have a gaussian proposal distribution of width 1
        testProp :: RandomGen g => [Double] -> ([Double], g) -> ([Double], g)
        testProp [] ([], gen) = ([], gen)
        testProp (mu:mus) (x:xs, gen) =
            let (x', gen'') = sampleGaussian ((x, mu), gen') in (x':xs', gen'')
            where
                (xs', gen') = testProp mus (xs, gen) 
        testProp _ _ = undefined

        testState :: RandomGen g => [Double] -> RandomWalk g [Double]
        testState mus = state $ runMCMC (testProp mus) (circleProb 1)
        -- testState = state $ runMCMC testProp (gaussianProb [(0, 1), (10, 4)])
