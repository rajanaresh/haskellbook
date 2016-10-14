module Arith4 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
  print (roundTrip "hello"::Int)
  print (id 4)

