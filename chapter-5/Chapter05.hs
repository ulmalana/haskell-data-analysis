module Chapter05 where 

import Math.Combinatorics.Exact.Binomial
import Chapter02
import Chapter04

probabilityMassFunction :: Integral a => a -> a -> Double -> Double
probabilityMassFunction k n p = (fromIntegral (n `choose` k)) * (p^k) * ((1-p)^(n-k))