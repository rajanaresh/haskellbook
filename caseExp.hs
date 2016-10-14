 module CaseExpression where


pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

pal' xs =
  case y of
    True  -> "yes"
    False -> "no"
  where y = xs == reverse xs

