module Chapter06 where

import Data.List
import Graphics.EasyPlot
import Chapter02
import Chapter04
import Chapter05

covariance :: [Double] -> [Double] -> Double
covariance x y = avg $ zipWith (\xi yi -> (xi - xavg) * (yi - yavg)) x y 
  where 
    xavg = avg x 
    yavg = avg y

pearsonR :: [Double] -> [Double] -> Double
pearsonR x y = r 
  where 
    xstdev = standardDeviation x 
    ystdev = standardDeviation y 
    r = covariance x y / (xstdev * ystdev)

pearsonRSqrd :: [Double] -> [Double] -> Double
pearsonRSqrd x y = pearsonR x y ^ 2

linearRegression :: [Double] -> [Double] -> (Double, Double)
linearRegression x y = (gradient, intercept)
  where
    xavg = avg x
    yavg = avg y
    xstdev = standardDeviation x
    gradient = covariance x y / (xstdev * xstdev)
    intercept = yavg - gradient * xavg