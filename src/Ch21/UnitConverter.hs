module Ch21.UnitConverter
  ( computePrice,
    computePrice',
  )
where

import Control.Monad.Reader
import Money

computePrice ::
  Dense src ->
  Integer ->
  Reader (ExchangeRate src dst) (Dense dst)
computePrice price quantity = do
  rate <- ask
  return $ exchange rate (price * fromIntegral quantity)

computePrice' ::
  Dense src ->
  Integer ->
  Reader (ExchangeRate src dst) (Dense dst)
computePrice' price quantity =
  reader $ \rate -> exchange rate (price * fromIntegral quantity)
