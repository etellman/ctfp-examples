module Ch09.Cartesian
  ( toSum,
    fromSum,
    fromProduct,
    toProduct,
  )
where

toSum :: (Int -> String) -> (Double -> String) -> (Either Int Double -> String)
toSum f _ (Left n) = f n
toSum _ g (Right x) = g x

fromSum :: (Either Int Double -> String) -> ((Int -> String), (Double -> String))
fromSum f =
  let intF n = f (Left n)
      doubleF x = f (Right x)
   in (intF, doubleF)

fromProduct :: (Int -> (Double, String)) -> ((Int -> Double), (Int -> String))
fromProduct f =
  let f1 n = fst $ f n
      f2 n = snd $ f n
   in (f1, f2)

toProduct :: (Int -> Double) -> (Int -> String) -> (Int -> (Double, String))
toProduct f g =
  let fg n = (f n, g n)
   in fg
