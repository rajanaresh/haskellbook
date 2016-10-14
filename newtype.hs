{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Newtype where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int
              deriving (Eq, Show, TooMany)

-- instance TooMany Goats where
--   tooMany (Goats n) = tooMany n

instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

newtype Product = Product (Int, String)
                  deriving (Eq, Show, TooMany)


instance TooMany (Int, Int) where
  tooMany (x, y) = x + y > 42

instance (Num a, TooMany a, Ord a) => TooMany (a, a) where
  tooMany (x, y) = x + y > 43      -- clash with (Int, Int)

