-- | What's the Strong typeclass good for?
module Strong where

import Control.Category
import Data.Profunctor
import ScratchPrelude hiding ((.))
import Test.Hspec
import UnliftIO.Exception

-- | Say we want to create an EDSL for deciding
-- whether or not to restart a machine in a factory.
--
-- We make an abstract type for this (in order to prevent users
-- from absentmindedly doing unexpected IO):
newtype Action a b
  = Internal (a -> IO b)

-- * Our EDSL contains three actions

-- | Look up the machine's identifier.
getMachineId :: Action () Id
getMachineId =
  -- A pretend implemenation. If the example was real this would
  -- do IO and find the Id of the machine on the factory's network.
  Internal (\() -> pure (Id "machine 1"))

-- | Using the machine's identifier, look up its sensor A reading.
checkSensorA :: Action Id Int
checkSensorA =
  Internal f
  where
    -- Pretend implemention.
    f :: Id -> IO Int
    f = \case
      Id "machine 1" -> pure 450
      _ -> throwString "unknown"

-- | Same, but for sensor B.
checkSensorB :: Action Id Int
checkSensorB =
  Internal f
  where
    -- Pretend implemention.
    f :: Id -> IO Int
    f = \case
      Id "machine 1" -> pure 50
      _ -> throwString "unknown"

newtype Id = Id Text

-- * Interface

-- | Users should be able to compose actions.
--
-- (Instead of using typeclasses like @Category@ we could write
-- ad hoc functions like @actionIdentity@, @composeActions@, etc.
-- This is nicer though.)
instance Category Action where
  id :: Action a a
  id =
    Internal pure

  (.) :: Action b c -> Action a b -> Action a c
  (.) (Internal f) (Internal g) =
    Internal (f <=< g)

-- | Users should be able to map over results and inputs.
-- Perhaps they know that Sensor A's readings should be capped
-- at 500 or whatever.
instance Profunctor Action where
  lmap :: (a -> b) -> Action b c -> Action a c
  lmap f (Internal g) =
    Internal (g . f)

  rmap :: (b -> c) -> Action a b -> Action a c
  rmap f (Internal g) =
    Internal (fmap f . g)

-- | Run the @Action@.
restartMachine :: Action () Bool -> IO Bool
restartMachine (Internal f) =
  f ()

-- * Let's try it out.
--
-- In the body of the examples we're looking at things
-- from the user's perspective, so we won't use @Internal@.

example1 :: Action () Bool
example1 =
  rmap decide (getMachineId >>> checkSensorA)
  where
    decide :: Int -> Bool
    decide n = n > 400

-- For the next example we'd like to use both sensors, eg:
--
-- 1. getMachineId              ::: Id
-- 2. duplicate to a tuple      ::: (Id, Id)
-- 3. getSensorA over the left  ::: (Int, Id)
-- 4. getSensorB over the right ::: (Int, Int)
-- 5. make a decision           ::: Bool

-- Here we hit a problem. We don't have a way to apply an
-- Action over just the left or the right.

-- | However, we can add a function to our interface for this.
overLeft :: Action a b -> Action (a, c) (b, c)
overLeft (Internal f) =
  Internal (\(a, c) -> fmap (\b -> (b, c)) (f a))

-- | And this is what it means to be a Strong profunctor!
instance Strong Action where
  first' :: Action a b -> Action (a, c) (b, c)
  first' = overLeft

-- "A profunctor is strong if it can freely pass unknown values through it without modification."
--
--     - https://elvishjerricco.github.io/2017/03/10/profunctors-arrows-and-static-analysis.html
--
-- Note that not all profunctors are strong. For example:
--
--     newtype Ignore a b = Ignore b
--
-- (from here: https://blog.functorial.com/posts/2015-12-06-Counterexamples.html).

-- | Now we can write our second example.
example2 :: Action () Bool
example2 =
  -- (When we implemented @first'@ we got @second'@ for free)
  rmap decide (rmap duplicate getMachineId >>> first' checkSensorA >>> second' checkSensorB)
  where
    duplicate :: a -> (a, a)
    duplicate a =
      (a, a)
    decide :: (Int, Int) -> Bool
    decide (aVal, bVal) =
      aVal + bVal > 800

spec :: Spec
spec = do
  describe "Machine DSL" $ do
    it "example1" $ do
      restartMachine example1 `shouldReturn` True
    it "example2" $ do
      restartMachine example2 `shouldReturn` False
