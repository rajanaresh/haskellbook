module Database where

import Data.Time
import Data.Bool

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
           (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = (map m) . (filter f)
  where f (DbDate _) = True
        f _          = False
        m (DbDate x) = x

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = (map m) . (filter f)
  where f (DbNumber _) = True
        f _            = False
        m (DbNumber x) = x

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr1 (\x y -> bool y x (x > y)) . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr1 (\x y -> x + y) . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb ds = fromIntegral (sumDb ds) / fromIntegral (length $ filterDbNumber ds)
