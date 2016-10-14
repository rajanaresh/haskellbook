module GlobalLocal where

topLevelFunction :: Integer -> Integer
topLevelFunction x = x + woot + topLevelValue + yay
  where woot :: Integer
        woot = 10
        yay :: Integer
        yay = 8

topLevelValue :: Integer
topLevelValue = 5


