module Ch14.Representable (listAlpha) where

-- not representable, since listBeta can't be defined
listAlpha :: (Int -> x) -> [x]
listAlpha h = fmap h [12]
