{-# LANGUAGE RankNTypes #-}

module Main where

import System.Random.MWC.Probability (Gen)
import qualified System.Random.MWC.Probability as MWC
import Control.Monad.Trans.State.Strict (execStateT)

import Statistics.Distribution.Poisson

import Conduit

import Data.Model
import Numeric.MCMC

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


main :: IO ()
main = withSystemRandom . asGenIO $
        \g -> chain (slice 1) c g =$ takeC 99999 =$ mapC (filter (`notElem` "|\"") . show) $$ mapM_C putStrLn

    where c = Chain t (testLH initPred) initPred Nothing
          t = Target testLH Nothing
          testData = [1, 100, 25, 50]
          initPred = [1, 1, 1, 1]
          testLH xs = if any (<= 0) xs
                         then (-1e100)
                         else predHistLLH testData (fmap poisson xs)
