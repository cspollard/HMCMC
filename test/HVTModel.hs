{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

import Data.List (dropWhileEnd)

import System.Random.MWC.Probability (Gen)
import qualified System.Random.MWC.Probability as MWC
import Control.Monad.Trans.State.Strict (execStateT)

import Statistics.Distribution.Normal
import Statistics.Distribution.Uniform

import Conduit
import System.Environment (getArgs)

import Data.Model
import Numeric.MCMC

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import qualified Data.ByteString as BS (readFile)
import Data.Aeson

-- ~from https://hackage.haskell.org/package/declarative-0.2.1/docs/src/Numeric-MCMC.html#mcmc
-- A Markov chain driven by an arbitrary transition operator.
-- now using Conduit instead of Pipes
chain
  :: PrimMonad m
  => Transition m b
  -> b
  -> Gen (PrimState m)
  -> Producer m b
chain transition = loop where
  loop state prng = do
    next <- lift (MWC.sample (execStateT transition state) prng)
    yield next
    loop next prng


takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd f xs = case span f xs of
                         (ys, [])    -> ys
                         (_, (_:ys)) -> takeWhileEnd f ys

takeEveryC :: Monad m => Int -> Conduit a m a
takeEveryC n = do dropC (n-1)
                  x <- await
                  case x of
                       Just y -> yield y >> takeEveryC n
                       Nothing -> return ()

showList' :: Show a => [a] -> String
showList' = filter (flip notElem ("[]" :: String)) . show

regions :: [RegName]
regions = [ "lvJ_1tag_0addtag_unblind_SR"
          , "lvJ_2tag_0addtag_unblind_SR"
          , "lvJ_1tag_1paddtag_unblind_SR"
          , "lvJ_1tag_0addtag_unblind_lowMH"
          , "lvJ_2tag_0addtag_unblind_lowMH"
          , "lvJ_1tag_1paddtag_unblind_lowMH"
          , "lvJ_1tag_0addtag_unblind_highMH"
          , "lvJ_2tag_0addtag_unblind_highMH"
          , "lvJ_1tag_1paddtag_unblind_highMH"
          ]

processes :: [ProcName]
processes  = [ "TTbar"
             , "Wb"
             , "Wc"
             , "Wl"
             , "stopWt"
             , "stopt"
             , "HVTWHlvqq2000" 
             ]

shapeNPs :: [String]
shapeNPs = ["EG_RESOLUTION_ALL__1up"]

main :: IO ()
main = do infiles <- getArgs
          let f Nothing = error "parse error."
              f (Just m) = fmap (rebinH 10 . snd) m

          let g k = let (procN, regN) = break (== '_') . takeWhileEnd (/= '/') . dropWhileEnd (/= '_') $ k
                    in  (ProcName procN, RegName . init . tail $ regN)

          fhs <- traverse (\fn -> (fn,) . decodeStrict <$> BS.readFile fn) infiles
                :: IO [(String, Maybe (Map String (String, Hist)))]

          
          let hs' = M.fromList $ map (\(ss, m) -> (g ss, f m)) fhs

          let regH regname procname varname = traceShow (regname, procname, varname) $! hs' M.! (procname, regname) M.! varname
          let procHs procname varname = M.fromList [(regn, regH regn procname varname) | regn <- regions]
          let varHs varname = M.fromList [(procn, procHs procn varname) | procn <- processes]

          let systHs = M.fromList $ map (\v -> (v, varHs v)) shapeNPs

          let nomHs = M.fromList $ map (\p -> (p, procHs p "nominal")) processes :: Prediction
          let dataHs = M.fromList $ map (\r -> (r, fmap round $ regH r "data" "nominal")) regions :: Dataset

          let sysDiffs = fmap (M.intersectionWith (M.intersectionWith (flip subH)) nomHs) systHs

          let shapeSysts = map (\(n, p) -> shapeParam n standard p) $ M.toList sysDiffs
          let normSysts = [ procNormParam (uniformDistr (-100.0) 100.0) "HVTWHlvqq2000"
                          , procNormParam (normalDistr 0.0 0.3) "TTbar"
                          , procNormParam (normalDistr 0.0 0.3) "Wb"
                          , procNormParam (normalDistr 0.0 0.3) "Wc"
                          , procNormParam (normalDistr 0.0 0.3) "Wl"
                          ]

          let systs = normSysts ++ shapeSysts 

          putStrLn . showList' . ("LL" :) $ map mpName systs

          -- start every NP at 0.0.
          let initial = map (const 0.0) systs

          let llh = modelLLH dataHs nomHs systs :: [Double] -> Double

          let t = Target llh Nothing
          let c = Chain t (llh initial) initial Nothing

          -- TODO
          -- I think the signal normalization needs a different step
          -- size...
          let trans = metropolis 0.005
          -- let trans = slice 10

          withSystemRandom . asGenIO $
                \gen -> chain trans c gen
                     =$ (dropC 10000 >> takeEveryC 50 =$ takeC 100000)
                     $$ mapM_C (\(Chain _ llhood xs _) -> putStrLn . showList' $ llhood:xs)
