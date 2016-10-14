module Chapter3Exercise where

appendEx :: String -> String
appendEx x = x ++ "!"

fifth :: String -> String
fifth x = [x !! 4]

drop9 :: String -> String
drop9 x = drop 9 x

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

