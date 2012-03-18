{-# LANGUAGE Rank2Types, ImpredicativeTypes, FlexibleContexts #-}
module Data.Vector.Conduit
    (
    sourceVector,
    sourceMVector,
    consumeVector,
    consumeMVector
    )
where

import Control.Monad.Primitive
import Control.Monad.ST.Safe
import Control.Monad.Trans.Class
import Data.Conduit
import qualified Data.DList as D
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M

-- | Use an immutable vector as a source.
sourceVector :: (Resource m, V.Vector v a) => v a -> Source m a
sourceVector v = sourceState 0 f
    where f index | index == V.length v = return StateClosed
                  | otherwise = return $ StateOpen (index + 1) (v V.! index)

-- | Use a mutable vector as a source in the ST or IO monad.
sourceMVector :: (PrimMonad m, Resource m, M.MVector v a)
                 => v (PrimState m) a
                 -> Source m a
sourceMVector v = sourceState 0 f
    where f index | index == M.length v = return StateClosed
                  | otherwise = do x <- lift $ M.read v index
                                   return $ StateOpen (index + 1) x

-- | Consumes all values from the stream and return as an immutable vector.
-- Works by creating a DList.
consumeVector :: (Resource m, V.Vector v a) => Sink a m (v a)
consumeVector = sinkState D.empty push close
    where push xs x = return . StateProcessing $ D.snoc xs x
          close xs = return . V.fromList . D.toList $ xs

-- | Consume all values from the stream and return as a mutable vector.
consumeMVector :: (PrimMonad m, Resource m, M.MVector v a)
                  => Sink a m (v (PrimState m) a)
consumeMVector = sinkState (Nothing, 0) push close
    where push (v, index) x = do v' <- case v of
                                        Nothing -> lift $ M.new 10
                                        Just vec -> return vec
                                 let len = M.length v'
                                 v'' <- if index >= len
                                            then lift $ M.grow v' len
                                            else return v'
                                 lift $ M.write v'' index x
                                 return $ StateProcessing (Just v'', index + 1)
          close (Nothing, index) = lift $ M.new 0
          close (Just v, index) = return $ M.take index v
