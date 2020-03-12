module TypeclassComparison where

import Control.Category (Category)
import Prelude (Applicative, Functor, Monad)

import qualified Control.Category as Category
import qualified Prelude

($)   ::                    (a ->   b) ->     a ->     b
fmap  :: Functor     f =>   (a ->   b) -> f   a -> f   b
(<*>) :: Applicative f => f (a ->   b) -> f   a -> f   b
(=<<) :: Monad       f =>   (a -> f b) -> f   a -> f   b
(<<<) :: Category    c => c  a      b  -> c x a -> c x b

($) = (Prelude.$)
fmap = Prelude.fmap
(<*>) = (Prelude.<*>)
(=<<) = (Prelude.=<<)
(<<<) = (Category.<<<)
