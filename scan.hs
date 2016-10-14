module Scan where

myScanl :: (a -> b -> a) -> a -> [b] -> [a]
myScanl f q ls = 
  q : (case ls of
         []   -> []
         x:xs -> scanl f (f q x) xs)

fibs = 1 : scanl (+) 1 fibs
fibN n = fibs !! n

fibs20 = take 20 fibs
fibs100 = [x | x <- take 100 fibs, x<100]

fact = scanl1 (*) [1..]

