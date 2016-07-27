{-# LANGUAGE RankNTypes #-}

module Main where

import System.Random.MWC.Probability (Gen, withSystemRandom, asGenIO)
import qualified System.Random.MWC.Probability as MWC
import Control.Monad.Trans.State.Strict (execStateT)


import Statistics.Distribution.Poisson
import Statistics.Quantile

import Data.Sampling.Types (Transition(..), Target(..))

import Data.HEPModel

import Conduit

import Numeric.MCMC

-- ~from https://hackage.haskell.org/package/declarative-0.2.1/docs/src/Numeric-MCMC.html#mcmc

-- A Markov chain driven by an arbitrary transition operator.
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
        \g -> chain (slice 1) c g =$ takeC 99999 $$ printC

    where c = Chain t (testLH [1, 1, 1, 1]) [1, 1, 1, 1] Nothing
          t = Target testLH Nothing
          testData = [1, 50, 25, 100]
          testLH xs = if any (<= 0) xs
                         then (-1e100)
                         else modelLLH testData (map poisson xs)
