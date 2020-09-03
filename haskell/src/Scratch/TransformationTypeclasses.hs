{- ORMOLU_DISABLE -}
module Scratch.TransformationTypeclasses where

import Control.Category (Category)
import Control.Comonad (Comonad)
import Control.Selective (Selective)
import Data.Function ((&))
import Data.Functor.Contravariant (Contravariant)
import Data.Functor.Contravariant.Divisible (Divisible)
import Prelude (Applicative, Either, Functor, Monad)

import qualified Control.Category as Category
import qualified Control.Comonad as Comonad
import qualified Control.Selective as Selective
import qualified Data.Functor.Contravariant as Contravariant
import qualified Data.Functor.Contravariant.Divisible as Divisible
import qualified Prelude

($)        ::                             (a ->    b) ->        a ->        b
fmap       :: Functor     f =>            (a ->    b) -> f      a -> f      b
(<*>)      :: Applicative f =>  f         (a ->    b) -> f      a -> f      b
(=<<)      :: Monad       f =>            (a -> f  b) -> f      a -> f      b
extend     :: Comonad     f => (f          a ->    b) -> f      a -> f      b
(.)        ::                             (a ->    b) -> (->) x a -> (->) x b
(<<<)      :: Category    c =>  c          a       b  -> c    x a -> c    x b
swapSelect :: Selective   f =>  f (Either (a -> b) b) -> f      a -> f      b

contramap :: Contravariant f => (b ->  a    ) -> f a        -> f b
divide    :: Divisible     f => (b -> (a, x)) -> f a -> f x -> f b

($) = (Prelude.$)
fmap = Prelude.fmap
(<*>) = (Prelude.<*>)
(=<<) = (Prelude.=<<)
extend = Comonad.extend
(.) = (Prelude..)
(<<<) = (Category.<<<)
swapSelect fEither = Selective.select fEither . fmap (&)

contramap = Contravariant.contramap
divide = Divisible.divide
