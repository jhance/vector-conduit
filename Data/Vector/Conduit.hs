module Data.Vector.Conduit
where

import Data.Conduit
import qualified Data.Vector.Generic.Safe as V
import qualified Data.Vector.Generic.Mutable.Safe as M

-- | Use an immutable vector as a source.
sourceVector :: (Resource m, V.Vector v a) => v a -> Source m a
sourceVector v = sourceState 0 f
    where f index | index == V.length v = return StateClosed
                  | otherwise = return $ StateOpen (index + 1) (v V.! index)
