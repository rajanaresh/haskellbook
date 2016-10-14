module ChangeMood where

data Mood = Woot | Blah deriving Show

changeMood Blah = Woot
changeMood _    = Blah
