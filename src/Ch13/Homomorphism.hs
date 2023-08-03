module Ch13.Homomorphism
  ( P (..),
    listToP,
    S (..),
    listToS,
    p,
    q,
    h,
  )
where

newtype P = P Int deriving (Eq, Show)

instance Semigroup P where
  (P x) <> (P y) = P (x * y)

instance Monoid P where
  mempty = P 1

listToP :: [Int] -> P
listToP = P . foldr (*) 1

newtype S = S Int deriving (Eq, Show)

instance Semigroup S where
  (S x) <> (S y) = S (x + y)

instance Monoid S where
  mempty = S 0

listToS :: [Int] -> S
listToS = S . foldr (+) 0

p :: Int -> ([Int] -> [Int])
p x = (++ [x])

q :: Int -> (Int -> Int)
q x = (+ x)

h :: ([Int] -> [Int]) -> (Int -> Int)
h f = \x -> (sum . f) [x]
