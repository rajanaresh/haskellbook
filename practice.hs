module Practice where

mult1     = x * y
  where x = 5
        y = 6

waxOn = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7

waxOff x = triple x `div` 10

triple x = x * 3


x `add` y = x + y

mul x y = x * y

test = 2 `add` 2 `mul` 4
