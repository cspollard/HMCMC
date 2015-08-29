module Main where

import GSL.Random.Dist
import System.Random (RandomGen, Random(..), mkStdGen)


type Sample g a b = (a, g) -> (b, g)


sampleUniform :: RandomGen g => Sample g (Double, Double) Double
sampleUniform ((mn, mx), gen) = (x', gen')
    where
        (x, gen') = random gen
        x' = (x * (mx - mn)) + mn


-- TODO
-- this only works out to 12 sigma.
sampleGaussian :: RandomGen g => Sample g (Double, Double) Double
sampleGaussian ((mu, sigma), gen) = (x', gen')
    where 
        (x, gen') = sampleUniform ((-12, 12), gen)
        x' = gaussianPInv 1.0 x * sigma + mu


sampleMany :: RandomGen g => ((a, g) -> (b, g)) -> (a, g) -> [b]
sampleMany f (a, g) = let (b, g') = f (a, g) in b : sampleMany f (a, g')


gaussianPdfMuSigma :: Double -> Double -> Double -> Double
gaussianPdfMuSigma mu sigma x = ugaussianPdf $ (x - mu) / sigma


{-
mcmc :: RandomGen g => (a, g) -> (a, g)
mcmc (x, g) = 
    where
        dx = sampleGaussianMuSigma gen 0 0.1
        let alpha = gaussianPdfMuSigma 0 1 (x+dx) / gaussianPdfMuSigma 0 1 x
        n <- getFlat gen 0 1

        if alpha > n
            then return $ (x+dx) : xs
            else return $ x : xs

mcmc _ [] = return []


iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ a = return a
iterateM n f a = f a >>= iterateM (n-1) f
-}


main :: IO ()
-- main = mapM_ print . take 100 $ sampleMany sampleGaussian ((15, 15), mkStdGen 0)
main = mapM_ print . take 1000000 $ sampleMany sampleUniform ((-15, 15), mkStdGen 0)

{-
    rng <- newRNG mt19937
    mapM_ print =<< iterateM 1000000 (mcmc rng) [0]
-}
