module Garden where

data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String

data Garden = Garden Gardener FlowerType
              deriving Show

type Gardener' = String

data FlowerType' = Gardenia' Gardener'
                 | Daisy' Gardener'
                 | Rose' Gardener'
                 | Lilac' Gardener'
                 deriving Show



data Garden' = Garden' FlowerType'
              deriving Show
