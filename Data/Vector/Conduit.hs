{-# LANGUAGE Rank2Types, ImpredicativeTypes, FlexibleContexts #-}
module Data.Vector.Conduit
    (
    sourceVector,
    sourceMVector,
    consumeVector,
    consumeMVector,
    takeVector,
    takeMVector,
    thawConduit,
    freezeConduit
    )
where

import Control.Monad.Primitive
import Control.Monad.ST.Safe
import Control.Monad.Trans.Class
import Data.Conduit
import qualified Data.Conduit.List as L
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
consumeVector :: (PrimMonad m, Resource m, V.Vector v a)
                 => Sink a m (v a)
consumeVector = sinkState (Nothing, 0) push close
    where push (v, index) x = do v' <- case v of
                                        Nothing -> lift $ M.new 10
                                        Just vec -> return vec
                                 let len = M.length v'
                                 v'' <- if index >= len
                                            then lift $ M.grow v' len
                                            else return v'
                                 lift $ M.write v'' index x
                                 return $ StateProcessing (Just v'', index + 1)
          close (Nothing, index) = return $ V.fromList []
          close (Just v, index) = lift . V.unsafeFreeze $ M.take index v

-- | Consumes the first n values from a source and returns as an immutable
-- vector.
takeVector :: (PrimMonad m, Resource m, V.Vector v a)
              => Int -> Sink a m (v a)
takeVector n = sinkState (Nothing, 0) push close
    where push (v, index) x = do
            v' <- case v of
                    Nothing -> lift $ M.new n
                    Just vec -> return vec
            if index >= n
                then do v'' <- lift $ V.unsafeFreeze v'
                        return $ StateDone Nothing v''
                else do lift $ M.write v' index x
                        return $ StateProcessing (Just v', index + 1)
          close (Nothing, index) = return $ V.fromList []
          close (Just v, index) = lift $ V.unsafeFreeze v

-- | Consumes all values from the stream and returns as a mutable vector.
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

-- | Consumes the first n values from the stream and returns as a
-- mutable vector.
takeMVector :: (PrimMonad m, Resource m, M.MVector v a)
               => Int -> Sink a m (v (PrimState m) a)
takeMVector n = sinkState (Nothing, 0) push close
    where push (v, index) x =
            do v' <- case v of
                        Nothing -> lift $ M.new n
                        Just vec -> return vec
               if index >= n
                    then return $ StateDone Nothing v'
                    else do lift $ M.write v' index x
                            return $ StateProcessing (Just v', index + 1)
          close (Nothing, index) = lift $ M.new 0
          close (Just v, index) = return v

-- | Conduit which thaws immutable vectors into mutable vectors
thawConduit :: (PrimMonad m, Resource m, V.Vector v a)
                => Conduit (v a) m (V.Mutable v (PrimState m) a)
thawConduit = L.mapM V.unsafeThaw

-- | Conduit which freezes mutable vectors into immutable vectors
freezeConduit :: (PrimMonad m, Resource m, V.Vector v a)
                 => Conduit (V.Mutable v (PrimState m) a) m (v a)
freezeConduit = L.mapM V.unsafeFreeze
