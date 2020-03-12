module TransformationTypeclasses where

import Control.Category (Category)
import Control.Comonad (Comonad)
import Control.Selective (Selective)
import Data.Functor.Contravariant (Contravariant)
import Prelude (Applicative, Either, Functor, Monad, flip)

import qualified Control.Category as Category
import qualified Control.Comonad as Comonad
import qualified Control.Selective as Selective
import qualified Data.Functor.Contravariant as Contravariant
import qualified Prelude

($)        ::                     (a ->   b) ->           a    ->     b
fmap       :: Functor     f =>    (a ->   b) -> f         a    -> f   b
(<*>)      :: Applicative f =>  f (a ->   b) -> f         a    -> f   b
(=<<)      :: Monad       f =>    (a -> f b) -> f         a    -> f   b
extend     :: Comonad     f => (f  a ->   b) -> f         a    -> f   b
(<<<)      :: Category    c =>  c  a      b  -> c x       a    -> c x b
flipSelect :: Selective   f =>  f (a ->   b) -> f (Either a b) -> f   b

contramap :: Contravariant f => (b -> a) -> f a -> f b

($) = (Prelude.$)
fmap = Prelude.fmap
(<*>) = (Prelude.<*>)
(=<<) = (Prelude.=<<)
extend = Comonad.extend
(<<<) = (Category.<<<)
flipSelect = flip Selective.select

contramap = Contravariant.contramap
