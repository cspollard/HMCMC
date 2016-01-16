module Main where

import System.Random

import Data.HMCMC.Dist

import HEPModel
import qualified Data.Map as M

import qualified Data.ByteString.Lazy as BS
import Data.Aeson

import Data.Maybe (fromJust)
import Data.List (isInfixOf)

import Control.Applicative ((<$>))

bkgNorm :: String -> HEPModelParam
bkgNorm = procNormParam 3.0 (\x -> if x < 0 then 0.0 else gaussianLogProb (1.0, 0.3) x)


sigNorm :: String -> HEPModelParam
sigNorm = procNormParam 1.0 (\x -> if x < 0 || x > 10 then 0.0 else 1.0)


gaussProps :: RandomGen g => [Double] -> ([Double], g) -> ([Double], g)
gaussProps [] ([], gen) = ([], gen)
gaussProps (sigma:sigmas) (x:xs, gen) =
    let (x', gen'') = sampleGaussian ((x, sigma), gen') in (x':xs', gen'')
    where
        (xs', gen') = gaussProps sigmas (xs, gen) 
gaussProps _ _ = undefined


appMany :: [a -> b] -> a -> [b]
appMany fs x = ($ x) `fmap` fs

every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
                (y:ys) -> y : every n ys
                [] -> []


main :: IO ()
-- main = print . appMany [mean, mode, median, stddev] . map fst . take 999999 $ iterateS testState ((1, 1), mkStdGen 0)
main = do
    mc <- fmap fromJust (decode `fmap` BS.getContents :: IO (Maybe HEPPrediction))
    let (sigs, bkgs) = M.partitionWithKey (\k _ -> isInfixOf "HVT" k) mc
    let nbkg = M.size bkgs
    -- remove HVT from data sample
    let ds = expectedData (M.insert "1.8 TeV HVT"  (fmap (scaleH 0.01) (sigs M.! "1.8 TeV HVT")) bkgs)
    -- let ds = expectedData bkgs
    print ds
    print $ totalPrediction bkgs
    print sigs
    let bkgparams = bkgNorm <$> M.keys bkgs
    let startBkgParams = replicate nbkg 1.0

    mapM_ (\s -> print (fst s) >> print (bestFit (ds, uncurry M.insert s bkgs, sigNorm (fst s) : bkgparams) (0.0 : startBkgParams))) $ M.toList sigs
