module FunctionWithLet where

printInc2 n = let plusTwo = n + 2
              in print plusTwo

test = let x = 5
           y = 6
       in x * y
