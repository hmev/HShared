{-# LANGUAGE TypeFamilies #-}
module Element.Family where

data Family Doors :: * -> *
data instance Doors Int = Doors