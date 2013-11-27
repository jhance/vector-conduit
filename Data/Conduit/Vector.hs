{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module Data.Conduit.Vector
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
import Control.Monad.ST
import Control.Monad.Trans
import Data.Conduit
import Data.Conduit.Internal (Pipe(..) )
import qualified Data.Conduit.List as L
import Data.Conduit.Util
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Fusion.Stream as S
import qualified Data.Vector.Fusion.Stream.Monadic as SM


instance PrimMonad m => PrimMonad (ConduitM i o m) where
  type PrimState (ConduitM i o m) = PrimState m
  primitive f = lift (primitive f)
  internal _ = error "no implementation of internal for (ConduitM i o m)"

-- | Use an immutable vector as a source.
sourceVector :: (Monad m, V.Vector v a) => v a -> Producer m a
sourceVector = V.mapM_ yield

-- | Use a mutable vector as a source in the ST or IO monad.
sourceMVector :: (PrimMonad m, M.MVector v a)
                 => v (PrimState m) a
                 -> Producer m a
sourceMVector vec = SM.mapM_ yield (M.mstream vec)

-- | Consumes all values from the stream and return as an immutable vector.
-- Due to the way it operates, it requires the ST monad at the minimum,
-- although it can also operate IO. This is due to its dependency on
-- a mutable vector.
consumeVector :: (PrimMonad m, V.Vector v a)
                 => Consumer a m (v a)
consumeVector = loop SM.empty
  where
   loop v = await >>= maybe (lift $ V.unsafeFreeze =<< M.munstream v) (\x -> loop $ v `SM.snoc` x)

-- | Consumes the first n values from a source and returns as an immutable
-- vector.
takeVector :: (PrimMonad m, V.Vector v a)
              => Int -> Consumer a m (v a)
takeVector n = loop n SM.empty
  where
    loop 0 v = lift $ V.unsafeFreeze =<< M.munstream v
    loop n v = await >>= maybe (loop 0 v) (\x -> loop (n - 1) $ v `SM.snoc` x)

-- | Consumes all values from the stream and returns as a mutable vector.
consumeMVector :: (PrimMonad m, M.MVector v a)
                  => Consumer a m (v (PrimState m) a)
consumeMVector = loop SM.empty
  where
   loop v = await >>= maybe (lift $ M.munstream v) (\x -> loop $ v `SM.snoc` x)

-- | Consumes the first n values from the stream and returns as a
-- mutable vector.
takeMVector :: (PrimMonad m, M.MVector v a)
               => Int -> Consumer a m (v (PrimState m) a)
takeMVector n = loop n SM.empty
  where
    loop 0 v = lift $ M.munstream v
    loop n v = await >>= maybe (loop 0 v) (\x -> loop (n - 1) $ v `SM.snoc` x)

-- | Conduit which thaws immutable vectors into mutable vectors
thawConduit :: (PrimMonad m, V.Vector v a)
                => Conduit (v a) m (V.Mutable v (PrimState m) a)
thawConduit = L.mapM V.unsafeThaw

-- | Conduit which freezes mutable vectors into immutable vectors
freezeConduit :: (PrimMonad m, V.Vector v a)
                 => Conduit (V.Mutable v (PrimState m) a) m (v a)
freezeConduit = L.mapM V.unsafeFreeze
