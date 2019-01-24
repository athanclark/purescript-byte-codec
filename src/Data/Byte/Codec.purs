-- | This is a synthetic interface on top of the lowest-level view JavaScript has over bytes - integers between 0 and 255.
-- |
-- | The `Flag` system is useful for building a byte in a typesafe way; where each "flag" is actually a bit in the integer's value.
-- |
-- | The `toBinary` / `fromBinary` API is useful for deserializing into a monomorphic type - an array of bits (as `Boolean`s), where the index reflects the bit's significance (big-endian).
module Data.Byte.Codec
  ( FlagSet (..), nullFlagSet, flagSetValue
  , Flag (..), flagValue
  , class FlagLabel, f0, f1, f2, f3, f4, f5, f6, f7
  , addFlag, getFlag, getFlagUnsafe, toBinary, fromBinary
  ) where

import Prelude ((-), (+), ($), (==), (<), (<=), div, otherwise, mod, class Show, class Eq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe (..), fromJust)
import Data.Tuple (Tuple (..))
import Data.Int (pow)
import Data.Foldable (sum)
import Data.Typelevel.Num (D0,D1,D2,D3,D4,D5,D6,D7,D8)
import Data.Array (reverse, mapWithIndex)
import Data.Vec (Vec, fromArray, toArray)
import Data.Unfoldable (unfoldr)
import Prim.Row (class Cons, class Lacks)
import Partial.Unsafe (unsafePartial)

-- | The type parameter to a `FlagSet` is a row of typelevel numbers, representing the bits present in the set.
newtype FlagSet (fs :: # Type) = FlagSet Int
derive instance genericFlagSet :: Generic (FlagSet fs) _
derive newtype instance eqFlagSet :: Eq (FlagSet fs)
derive newtype instance showFlagSet :: Show (FlagSet fs)
nullFlagSet :: FlagSet ()
nullFlagSet = FlagSet 0
flagSetValue :: forall fs. FlagSet fs -> Int
flagSetValue (FlagSet x) = x
-- | The type parameter to a `Flag` must be a `Data.Typelevel.Num` natural number, between 0 and 7 - this also represents
-- | its index in the boolean array from `getBinary`.
newtype Flag f = Flag Int
derive instance genericFlag :: Generic (Flag fs) _
derive newtype instance eqFlag :: Eq (Flag fs)
derive newtype instance showFlag :: Show (Flag fs)
flagValue :: forall f. Flag f -> Int
flagValue (Flag x) = x

class FlagLabel f (s :: Symbol) | f -> s
instance flagLabelD0 :: FlagLabel D0 "0"
instance flagLabelD1 :: FlagLabel D1 "1"
instance flagLabelD2 :: FlagLabel D2 "2"
instance flagLabelD3 :: FlagLabel D3 "3"
instance flagLabelD4 :: FlagLabel D4 "4"
instance flagLabelD5 :: FlagLabel D5 "5"
instance flagLabelD6 :: FlagLabel D6 "6"
instance flagLabelD7 :: FlagLabel D7 "7"


-- | `2^0`
f0 :: Flag D0
f0 = Flag 1

-- | `2^1`
f1 :: Flag D1
f1 = Flag 2

-- | `2^2`
f2 :: Flag D2
f2 = Flag 4

-- | `2^3`
f3 :: Flag D3
f3 = Flag 8

-- | `2^4`
f4 :: Flag D4
f4 = Flag 16

-- | `2^5`
f5 :: Flag D5
f5 = Flag 32

-- | `2^6`
f6 :: Flag D6
f6 = Flag 64

-- | `2^7`
f7 :: Flag D7
f7 = Flag 128


addFlag :: forall f fs fs' fl
         . FlagLabel f fl
        => Lacks fl fs
        => Cons fl f fs fs'
        => Flag f
        -> FlagSet fs
        -> FlagSet fs'
addFlag (Flag f) (FlagSet x) = FlagSet (f + x)


getFlag :: forall f fs fs' fl
         . FlagLabel f fl
        => Cons fl f fs fs'
        => Flag f
        -> FlagSet fs'
        -> FlagSet fs
getFlag (Flag f) (FlagSet x) = FlagSet (x - f)


getFlagUnsafe :: forall f fs fs'
               . Flag f
              -> FlagSet fs'
              -> Maybe (FlagSet fs)
getFlagUnsafe (Flag f) (FlagSet x)
  | x < f = Nothing
  | otherwise = Just (FlagSet (x - f))






-- | Returns a big-endian array of bits - Performs a `mod 256` of the input.
toBinary :: Int -> Vec D8 Boolean
toBinary x = unsafePartial $ fromJust $ fromArray $ reverse $ unfoldr go {val: x `mod` 256, flag: flagValue f7}
  where
    go :: {val :: Int, flag :: Int} -> Maybe (Tuple Boolean {val :: Int, flag :: Int})
    go {val,flag}
      | flag == 0 = Nothing -- set by 1
      | flag == 1 = Just (step 0 flag val) -- last round
      | otherwise = Just (step (flag `div` 2) flag val)
    step flag' flag val = case getValue flag val of
      Just val' -> Tuple true {val: val', flag: flag'}
      Nothing -> Tuple false {val, flag: flag'}
    getValue n y
      | n <= y = Just (y - n)
      | otherwise = Nothing

-- | Returns a byte value from an array of big-endian bits.
fromBinary :: Vec D8 Boolean -> Int
fromBinary xs = sum (mapWithIndex go (toArray xs))
  where
    go i j
      | j = 2 `pow` i
      | otherwise = 0
