module Main where

import Numeric.MCMC.Metropolis
import Statistics.Distribution
import Statistics.Distribution.Poisson
import Data.HEPModel

main :: IO ()
main = withSystemRandom . asGenIO $ mcmc 99999 0.1 [1, 1, 1, 1] testLH
    where testData = [1, 2, 3, 4]
          testLH xs = if any (<= 0) xs
                         then (-1.0/0.0)
                         else modelLLH testData (map poisson xs)
