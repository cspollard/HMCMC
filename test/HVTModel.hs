{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Random.MWC.Probability (Gen, withSystemRandom, asGenIO)
import qualified System.Random.MWC.Probability as MWC
import Control.Monad.Trans.State.Strict (execStateT)

import Statistics.Distribution.Poisson
import Statistics.Distribution.Normal
import Statistics.Distribution.Uniform
import Data.Foldable (traverse_)

import Conduit
import System.Environment (getArgs)

import Data.Model
import Numeric.MCMC
import Data.Sampling.Types (Transition(..), Target(..))

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
                         (_, (y:ys)) -> takeWhileEnd f ys

takeEveryC :: Monad m => Int -> Conduit a m a
takeEveryC n = do dropC (n-1)
                  x <- await
                  case x of
                       Just y -> yield y >> takeEveryC n
                       Nothing -> return ()

main :: IO ()
main = do infiles <- getArgs
          fhs <- traverse (\f -> (f,) . decodeStrict <$> BS.readFile f) infiles
                :: IO [(String, Maybe (Map String (String, Hist)))]

          let g (_, Nothing)  = error "failed parse."
              g (k, (Just m)) = let (procN, regN) = break (== '_') $ takeWhileEnd (/= '/') k
                                in  flip M.map m $ M.singleton (ProcName procN)
                                                    . M.singleton (RegName regN)
                                                    . fmap (\x -> if x < 0 then 1.0e-10 else x)
                                                    . rebinH 10 . snd

          let hs = M.unionsWith (M.unionWith M.union) $ map g fhs

          let dataset = fmap (fmap round) $ hs M.! "nominal" M.! "data" :: Dataset

          let nomH = M.delete "data" $ hs M.! "nominal"

          let nomPred = fmap (fmap (fmap poisson)) nomH :: Prediction

          let sysHs = M.delete "nominal" hs

          let sysPreds = fmap (M.unionWith (M.unionWith (zipWith (flip (/)))) nomH) sysHs

          let mps = map (shapeParam standard) $ M.elems sysPreds :: [ModelParam]
          let mps' = procNormParam (uniformDistr (-10.0) 100.0) "HVTWHlvqq2000" : mps

          putStrLn . filter (flip notElem ("[]" :: String)) . show $ "HVTWHlvqq2000" : M.keys sysPreds

          let nmp = length mps'
          let init = replicate nmp 0

          let t = Target (modelLLH dataset nomPred mps') Nothing
          let c = Chain t (modelLLH dataset nomPred mps' init) init Nothing

          -- TODO
          -- slice isn't working!
          -- let trans = slice 0

          let trans = metropolis 0.1

          withSystemRandom . asGenIO $
                \g -> chain trans c g
                   =$ takeEveryC 1
                   =$ takeC 100
                   $$ mapM_C print
