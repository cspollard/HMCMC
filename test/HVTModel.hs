{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (isSuffixOf)

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

main :: IO ()
main = do infiles <- getArgs
          fhs <- traverse (\f -> (f,) . decodeStrict <$> BS.readFile f) infiles
                :: IO [(String, Maybe (Map String (String, Hist)))]

          let g (_, Nothing)  = error "failed parse."
              g (k, (Just m)) = let (procN, regN) = break (== '_') $ takeWhileEnd (/= '/') k
                                in  flip M.map m $ M.singleton (ProcName procN)
                                                    . M.singleton (RegName regN)
                                                    . rebinH 10 . snd

          let hs = M.filterWithKey (\k _ -> not $ "__1down" `isSuffixOf` k) $
                        M.unionsWith (M.unionWith M.union) $ map g fhs

          let dataset = fmap (fmap round) $ hs M.! "nominal" M.! "data" :: Dataset

          let nomPred = M.delete "data" $ hs M.! "nominal" :: Prediction

          let sysHs = M.delete "nominal" hs

          let sysDiffs = fmap (M.intersectionWith (M.intersectionWith (flip subH)) nomPred) sysHs

          let shapeSysts = map (\(n, p) -> shapeParam n (normalDistr 0 1.0) p) $ M.toList sysDiffs
          let normSysts = [ procNormParam (uniformDistr 1.0e-10 100.0) "HVTWHlvqq2000"
                          , procNormParam (normalDistr 1.0 0.3) "TTbar"
                          , procNormParam (normalDistr 1.0 0.3) "Wb"
                          , procNormParam (normalDistr 1.0 0.3) "Wc"
                          , procNormParam (normalDistr 1.0 0.3) "Wl"
                          ]

          let systs = normSysts ++ shapeSysts 

          putStrLn . showList' $ "LL" : map mpName systs

          -- start every NP at 1.0.
          let initial = map (const 1.0) normSysts ++ map (const 0.0) shapeSysts

          let f = modelLLH dataset nomPred systs :: [Double] -> Double

          let t = Target f Nothing
          let c = Chain t (f initial) initial Nothing

          -- TODO
          -- I think the signal normalization needs a different step
          -- size...
          let trans = metropolis 0.25

          withSystemRandom . asGenIO $
                \gen -> chain trans c gen
                     =$ takeEveryC 10
                     =$ takeC 1000
                     $$ mapM_C (\(Chain _ llhood xs _) -> putStrLn . showList' $ llhood:xs)
