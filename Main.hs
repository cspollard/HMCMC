module Main where

import GSL.Random.Dist
import GSL.Random.Gen

sampleGaussianMuSigma :: RNG -> Double -> Double -> IO Double
sampleGaussianMuSigma gen mu sigma = do
    x <- getUGaussian gen
    return $ (x * sigma) + mu

gaussianPdfMuSigma :: Double -> Double -> Double -> Double
gaussianPdfMuSigma mu sigma x = ugaussianPdf $ (x - mu) / sigma


mcmc :: RNG -> [Double] -> IO [Double]
mcmc gen xs@(x:_) = do
    dx <- sampleGaussianMuSigma gen 0 0.1
    let alpha = gaussianPdfMuSigma 0 1 (x+dx) / gaussianPdfMuSigma 0 1 x
    n <- getFlat gen 0 1

    if alpha > n
        then return $ (x+dx) : xs
        else return $ x : xs

mcmc _ [] = return []


iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ a = return a
iterateM n f a = f a >>= iterateM (n-1) f


main :: IO ()
main = do
    rng <- newRNG mt19937
    mapM_ print =<< iterateM 1000000 (mcmc rng) [0]
