module Data.Byte.Codec where

import Prelude ((-), (+))
import Data.Typelevel.Num (D1,D2,D3,D4,D5,D6,D7,D8)
import Prim.Row (class Cons, class Lacks)


newtype Byte (contains :: # Type) = Byte Int
nullByte :: Byte ()
nullByte = Byte 0
newtype Flag f = Flag Int

class FlagLabel f (s :: Symbol) | f -> s
instance flagLabelD1 :: FlagLabel D1 "d1"
instance flagLabelD2 :: FlagLabel D2 "d2"
instance flagLabelD3 :: FlagLabel D3 "d3"
instance flagLabelD4 :: FlagLabel D4 "d4"
instance flagLabelD5 :: FlagLabel D5 "d5"
instance flagLabelD6 :: FlagLabel D6 "d6"
instance flagLabelD7 :: FlagLabel D7 "d7"
instance flagLabelD8 :: FlagLabel D8 "d8"


f1 :: Flag D1
f1 = Flag 1

f2 :: Flag D2
f2 = Flag 2

f3 :: Flag D3
f3 = Flag 4

f4 :: Flag D4
f4 = Flag 8

f5 :: Flag D5
f5 = Flag 16

f6 :: Flag D6
f6 = Flag 32

f7 :: Flag D7
f7 = Flag 64

f8 :: Flag D8
f8 = Flag 128


addFlag :: forall f fs fs' fl
         . FlagLabel f fl
        => Lacks fl fs
        => Cons fl f fs fs'
        => Flag f
        -> Byte fs
        -> Byte fs'
addFlag (Flag f) (Byte x) = Byte (f + x)


getFlag :: forall f fs fs' fl
         . FlagLabel f fl
        => Cons fl f fs fs'
        => Flag f
        -> Byte fs'
        -> Byte fs
getFlag (Flag f) (Byte x) = Byte (x - f)


getFlagUnsafe :: forall f fs fs' fl
               . Flag f
              -> Byte fs'
              -> Maybe (Byte fs)
getFlagUnsafe (Flag f) (Byte x)
  | x < f = Nothing
  | otherwise = Just (Byte (x - f))


type All = (d8 :: D8, d7 :: D7, d6 :: D6, d5 :: D5, d4 :: D4, d3 :: D3, d2 :: D2, d1 :: D1)


byte :: forall fs. Int -> Byte fs
byte i =
  let x :: Byte All
      x = Byte i
  in  case getFlagUnsafe f8 x of
    Just x' ->
    Nothing -> 


-- caste from 1 -> 0?


-- do bytes have endianess?
getBinary :: Int -> Vec D8 Boolean
