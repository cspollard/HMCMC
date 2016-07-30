module Statistics.Distribution.LogNormal ( logNormalDistr
                                         , logNormalDistr'
                                         , getNormal
                                         ) where

import Statistics.Distribution
import Statistics.Distribution.Normal

data LogNormalDistribution = LND { getNormal :: NormalDistribution }
    deriving (Eq, Read, Show)

logNormalDistr :: Double -> Double -> LogNormalDistribution
logNormalDistr m s = LND $ normalDistr m s

-- gives the approximate mean and stdDev for the LogNormal.
logNormalDistr' :: Double -> Double -> LogNormalDistribution
logNormalDistr' m s = LND $ normalDistr mu sigma
    where mu = log $ m*m / sqrt (s*s + m*m)
          sigma = sqrt . log $ 1 + ((s*s) / (m*m))


instance Distribution LogNormalDistribution where
    cumulative (LND nd) = cumulative nd . log

instance Mean LogNormalDistribution where
    mean (LND nd) = exp $ variance nd / 2 + mean nd

instance MaybeMean LogNormalDistribution where
    maybeMean = Just . mean

instance Variance LogNormalDistribution where
    variance lnd@(LND nd) = (exp (variance nd) - 1) * (mean lnd ** 2)

instance MaybeVariance LogNormalDistribution where
    maybeVariance = Just . variance

instance ContGen LogNormalDistribution where
    genContVar (LND nd) g = exp <$> genContVar nd g

instance ContDistr LogNormalDistribution where
    quantile (LND nd) = quantile nd . log
    density (LND nd) x | x <= 0     = 0.0
                       | otherwise = density nd (log x) / (x * sqrt (stdDev nd) )
