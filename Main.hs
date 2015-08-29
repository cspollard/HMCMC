module Main where

import GSL.Random.Dist
import System.Random (RandomGen, Random(..), mkStdGen)


type Sample g a = (a, g) -> (Double, g)


sampleUniform :: RandomGen g => Sample g (Double, Double)
sampleUniform ((mn, mx), gen) = (x', gen')
    where
        (x, gen') = random gen
        x' = (x * (mx - mn)) + mn

sampleGaussian :: RandomGen g => Sample g (Double, Double)
sampleGaussian = sampleCDFInv gaussianCDFInv


-- TODO
-- this only works out to 12 sigma.
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
main = mapM_ print . take 10000 $ sampleForever sampleGaussian ((10, 0.55), mkStdGen 0)
-- main = mapM_ print . take 1000000 $ sampleForever sampleUniform ((-15, 15), mkStdGen 0)

{-
    rng <- newRNG mt19937
    mapM_ print =<< iterateM 1000000 (mcmc rng) [0]
-}
