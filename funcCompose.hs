module FunctionCompose where

inc :: Num a => a -> a
inc x = x + 1
dec :: Num a => a -> a
dec x = x - 1

same :: Num a => a -> a
same = inc . dec

