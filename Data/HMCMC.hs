module Data.HMCMC where

import System.Random (RandomGen, Random(..), mkStdGen)
import Control.Monad.Trans.State.Lazy

import Data.HMCMC.Dist

-- a RandomWalk computes the next state based on the current state and
-- the state of the random number generator.
type RandomWalk g a = State (a, g) a

-- infinite iteration of Stateful computations
iterateS :: State s a -> s -> [a]
iterateS st s = let (v, s') = runState st s in v : iterateS st s'


runMCMC :: RandomGen g => ((a, g) -> (a, g)) -> (a -> Double) -> (a, g) -> (a, (a, g))
runMCMC propose prob (ps, gen) =
             if probPropose / probCurr > u
                then (ps', (ps', gen''))
                else (ps, (ps, gen''))
            where
                (ps', gen') = propose (ps, gen) 
                (u, gen'') = sampleUniform ((0, 1), gen')
                probCurr = prob ps
                probPropose = prob ps'

mcmc :: RandomGen g => ((a, g) -> (a, g)) -> (a -> Double) -> RandomWalk g a
mcmc propose prob = state $ runMCMC propose prob
