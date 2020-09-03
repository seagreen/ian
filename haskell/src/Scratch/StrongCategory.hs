module Scratch.StrongCategory where

import Control.Category
import Data.Align
import Data.These
import Scratch.Prelude hiding ((.))

class Strong f where
  first' :: f a b -> f (a, c) (b, c)

  second' :: f a b -> f (c, a) (c, b)

-- * Example

-- Model an assembly line using Haskell.
--
-- Assembly lines can't run Haskell code, so we can't make them Functors.
--
-- This example just models a tiny assembly line with two machines,
-- but you could extend it to much larger systems.

data AssemblyLine a b
  = AssemblyLine [Machine]

data Machine
  = -- | No op
    ConveyerBelt
  | SplitConveyerBelt
  | -- | Two conveyer belts or stations side-by-side
    Simultaneously Machine Machine
  | CoffeeRoaster
  | CoffeeBagger
  deriving (Show)

conveyerBelt :: AssemblyLine a a
conveyerBelt =
  AssemblyLine [ConveyerBelt]

splitConveyerBelt :: AssemblyLine a (a, a)
splitConveyerBelt =
  AssemblyLine [SplitConveyerBelt]

simultaneously :: AssemblyLine a b -> AssemblyLine c d -> AssemblyLine (a, c) (b, d)
simultaneously (AssemblyLine xs) (AssemblyLine ys) =
  AssemblyLine (alignWith f xs ys)
  where
    f :: These Machine Machine -> Machine
    f = \case
      This a ->
        a
      That a ->
        a
      These a b ->
        Simultaneously a b

instance Category AssemblyLine where
  id :: AssemblyLine a a
  id =
    conveyerBelt

  (.) :: AssemblyLine b c -> AssemblyLine a b -> AssemblyLine a c
  AssemblyLine xs . AssemblyLine ys =
    AssemblyLine (ys <> xs)

instance Strong AssemblyLine where
  first' :: AssemblyLine a b -> AssemblyLine (a, c) (b, c)
  first' al =
    simultaneously al id

  second' :: AssemblyLine a b -> AssemblyLine (c, a) (c, b)
  second' =
    simultaneously id

data GreenCoffeeBeans

data RoastedBeans

data EmptyBag

data FilledCoffeeBag

coffeeRoaster :: AssemblyLine GreenCoffeeBeans RoastedBeans
coffeeRoaster =
  AssemblyLine [CoffeeRoaster]

bagger :: AssemblyLine (RoastedBeans, EmptyBag) FilledCoffeeBag
bagger =
  AssemblyLine [CoffeeBagger]

coffeeProductionBlueprint :: AssemblyLine (GreenCoffeeBeans, EmptyBag) FilledCoffeeBag
coffeeProductionBlueprint =
  first' coffeeRoaster >>> bagger

printBlueprint :: AssemblyLine a b -> String
printBlueprint (AssemblyLine machines) =
  show machines
