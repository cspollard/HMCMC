module Main where

import System.Random

import Data.HMCMC
import Data.HMCMC.Dist

import HEPModel
import qualified Data.Map as M

import qualified Data.ByteString.Lazy as BS
import Data.Aeson

import Data.Maybe (fromJust)
import Data.List (isInfixOf)

bkgNorm :: String -> HEPModelParam
bkgNorm = procNormParam (\x -> if x < 0 then 0.0 else gaussianPdf (1.0, 0.3) x)


sigNorm :: String -> HEPModelParam
sigNorm = procNormParam (\x -> if x < 0 || x > 1 then 0.0 else 1)


gaussProps :: RandomGen g => [Double] -> ([Double], g) -> ([Double], g)
gaussProps [] ([], gen) = ([], gen)
gaussProps (sigma:sigmas) (x:xs, gen) =
    let (x', gen'') = sampleGaussian ((x, sigma), gen') in (x':xs', gen'')
    where
        (xs', gen') = gaussProps sigmas (xs, gen) 
gaussProps _ _ = undefined


appMany :: [a -> b] -> a -> [b]
appMany fs x = ($ x) `fmap` fs

every n xs = case drop (n-1) xs of
                (y:ys) -> y : every n ys
                [] -> []


main :: IO ()
-- main = print . appMany [mean, mode, median, stddev] . map fst . take 999999 $ iterateS testState ((1, 1), mkStdGen 0)
main = do
    mc <- fmap fromJust (decode `fmap` BS.getContents :: IO (Maybe HEPPrediction))
    let (bkgs, sigs) = M.partitionWithKey (\k _ -> not $ isInfixOf "HVT" k) mc
    let nbkg = M.size bkgs
    let nsig = M.size sigs
    -- remove HVT from data sample
    let ds = expectedData bkgs
    let params = fmap sigNorm (M.keys sigs) ++ (fmap bkgNorm $ M.keys bkgs)
    let testState = mcmc (gaussProps $ replicate (nbkg+nsig) 0.01) (modelLH ds mc params)
    let startParams = replicate nsig 0 ++ replicate nbkg 1.0
    let startProb = modelLH ds mc params startParams

    mapM_ (\xs -> mapM_ (\x -> putStr (show x) >> putChar ' ') xs >> putChar '\n') . take 99999 . every 5 $
            iterateS testState ((startParams, startProb), mkStdGen 0)
