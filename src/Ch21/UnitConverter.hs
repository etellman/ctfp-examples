module Ch21.UnitConverter
  ( Rates,
    computePrice,
    computePrice',
    reverseConvert,
  )
where

import Control.Monad.Reader
import Data.Map
import Money

-- conversion rate for a particular currency
type Rates = Map String Double

computePrice :: String -> Double -> (Reader Rates Double)
computePrice currency price = do
  rates <- ask
  return $ (rates ! currency) * price

-- without "do" notation
computePrice' :: String -> Double -> (Reader Rates Double)
computePrice' currency price = reader $ \rates -> (rates ! currency) * price

reverseConvert :: String -> Double -> (Reader Rates Double)
reverseConvert currency price = do
  rates <- ask
  return $ price / (rates ! currency)
