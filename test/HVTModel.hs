{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Random.MWC.Probability (Gen)
import qualified System.Random.MWC.Probability as MWC
import Control.Monad.Trans.State.Strict (execStateT)

import Statistics.Distribution.Poisson
import Statistics.Distribution.Normal
import Statistics.Distribution.Uniform

import Conduit
import System.Environment (getArgs)

import Data.Model
import Numeric.MCMC
import Numeric.AD (grad)

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
                                                    . fmap (\x -> if x < 0 then 1.0e-10 else x)
                                                    . rebinH 10 . snd

          let hs = M.unionsWith (M.unionWith M.union) $ map g fhs

          let dataset = fmap (fmap round) $ hs M.! "nominal" M.! "data" :: Dataset

          let nomH = M.delete "data" $ hs M.! "nominal"

          let nomPred = fmap (fmap (fmap poisson)) nomH :: Prediction

          let sysHs = M.delete "nominal" hs

          let sysPreds = fmap (M.intersectionWith (M.intersectionWith (zipWith (flip (/)))) nomH) sysHs

          let shapeSysts = fmap (shapeParam standard) sysPreds
          let normSysts = M.fromList [ ("HVT_norm", procNormParam (uniformDistr (-10.0) 100.0) "HVTWHlvqq2000")
                                     , ("tt_norm", procNormParam (normalDistr 1 0.3) "TTbar")
                                     , ("Wb_norm", procNormParam (normalDistr 1 0.3) "Wb")
                                     , ("Wc_norm", procNormParam (normalDistr 1 0.3) "Wc")
                                     , ("Wl_norm", procNormParam (normalDistr 1 0.3) "Wl")
                                     ]

          let m = M.union shapeSysts normSysts
          let systs = M.elems m
          let systNames = M.keys m

          putStrLn . showList' $ "LL" : systNames

          let initial = replicate (M.size shapeSysts) 0 ++ replicate (M.size normSysts) 1
          let f = modelLLH dataset nomPred systs :: [Double] -> Double
          let gradF = grad $ modelLLH dataset nomPred systs :: [Double] -> [Double]

          let t = Target f (Just gradF)
          let c = Chain t (f initial) initial Nothing

          -- TODO
          -- slice and hamiltonian aren't working!
          -- let trans = slice 0
          -- let trans = hamiltonian 0.01 10

          -- TODO
          -- I think the signal normalization needs a different step
          -- size...
          let trans = metropolis 0.1

          withSystemRandom . asGenIO $
                \gen -> chain trans c gen
                     =$ takeEveryC 20
                     =$ takeC 10000
                     $$ mapM_C (\(Chain _ ll xs _) -> putStrLn . showList' $ ll:xs)
