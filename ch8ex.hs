module Chapter8Exercise where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y


flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count | n < d     = (count, n)
                     | otherwise = go (n - d) d (count + 1)

recSum :: (Eq a, Num a) => a -> a
recSum 0 = 0
recSum n = n + recSum (n-1)

recMul :: Integral a => a -> a -> a
recMul mcant mlier = go mcant mlier 0
  where go mc ml result | ml == 0    = result
                        | otherwise  = go mc (ml - 1) (result + mc)


data DividedResult a = Result a
                     | DividedByZero deriving (Show)
        
dividedBy' :: (Integral a, Ord a) => a -> a -> DividedResult a
dividedBy' num denom = go (abs num) (abs denom) 0
  where go n d count | d == 0    = DividedByZero
                     | n < d     = Result $ fst (sign * count, n)
                     | otherwise = go (n - d) d (count + 1)
        sign | num * denom <= 0 = -1
             | otherwise        = 1


mc91 :: Integral a => a -> a
mc91 n | n > 100  = n -10
       | n <= 100 = mc91 $ mc91 (n+11)