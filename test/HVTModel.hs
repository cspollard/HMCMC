{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Random.MWC.Probability (Gen)
import qualified System.Random.MWC.Probability as MWC
import Control.Monad.Trans.State.Strict (execStateT)

import Statistics.Distribution.Poisson
import Statistics.Distribution.LogNormal
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
                                                    . fmap (\x -> if x <= 0 then 1.0e-10 else x)
                                                    . rebinH 10 . snd

          let hs = M.unionsWith (M.unionWith M.union) $ map g fhs

          let dataset = fmap (fmap round) $ hs M.! "nominal" M.! "data" :: Dataset

          let nomH = M.delete "data" $ hs M.! "nominal"

          let nomPred = fmap (fmap (fmap poisson)) nomH :: Prediction

          let sysHs = M.delete "nominal" hs

          let sysPreds = fmap (M.intersectionWith (M.intersectionWith (zipWith (flip (/)))) nomH) sysHs

          let shapeSysts = map (\(n, p) -> shapeParam n (logNormalDistr 1.0 1.0) p) $ M.toList sysPreds
          let normSysts = [ procNormParam (uniformDistr 1.0e-10 100.0) "HVTWHlvqq2000"
                          , procNormParam (logNormalDistr 1 0.3) "TTbar"
                          , procNormParam (logNormalDistr 1 0.3) "Wb"
                          , procNormParam (logNormalDistr 1 0.3) "Wc"
                          , procNormParam (logNormalDistr 1 0.3) "Wl"
                          ]

          let systs = normSysts ++ shapeSysts 

          putStrLn . showList' $ "LL" : map mpName systs

          -- start every NP at 1.0.
          let initial = map (const 1.0) systs

          let f = modelLLH dataset nomPred systs :: [Double] -> Double

          let t = Target f Nothing
          let c = Chain t (f initial) initial Nothing

          -- TODO
          -- I think the signal normalization needs a different step
          -- size...
          let trans = metropolis 0.1

          withSystemRandom . asGenIO $
                \gen -> chain trans c gen
                     =$ takeEveryC 20
                     =$ takeC 2000
                     $$ mapM_C (\(Chain _ ll xs _) -> putStrLn . showList' $ ll:xs)
