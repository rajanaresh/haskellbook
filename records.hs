module Records where

-- data Person = MkPerson String Int 
--               deriving (Eq, Show)

jm = Person "Julie" 30
ca = Person "Chris" 30


data Person = Person { name :: String
                     , age  :: Int } 
              deriving (Eq, Show)


