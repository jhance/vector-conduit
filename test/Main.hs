module Main (main)
where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Vector
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Test.HUnit
import Test.QuickCheck
import qualified Test.Framework as F
import Test.Framework.Runners.Console (defaultMain)
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

-- QuickCheck Properties
testInverse :: [Int] -> Bool
testInverse l = runST $ do let v = V.fromList l
                           v' <- sourceVector v $$ consumeVector
                           return $ v == v'

testInverse2 :: [Int] -> Bool
testInverse2 l = runST $ do let v = V.fromList l
                            v' <- sourceVector v $$ consumeMVector
                            v'' <- V.unsafeFreeze v'
                            return $ v == v''

testInverse3 :: [[Int]]-> Bool
testInverse3 ls = runST $ do let vs = map V.fromList ls
                             let v = V.fromList vs
                             v' <- sourceVector v $= thawConduit $= freezeConduit $$ consumeVector
                             return $ v == v'

-- HUnit Cases
testSourceVector' :: [Int] -> IO ()
testSourceVector' l = do let v = V.fromList l
                         l' <- sourceVector v $$ CL.consume
                         assertEqual "consumed list" l l'


testSourceVector :: IO ()
testSourceVector = testSourceVector' [1, 2, 6, 3]

testSourceVectorEmpty :: IO ()
testSourceVectorEmpty = testSourceVector' []

testSourceMVector' :: [Int] -> IO ()
testSourceMVector' l = do v <- V.unsafeThaw $ V.fromList l
                          l' <- sourceMVector v $$ CL.consume
                          assertEqual "consumed list" l l'

testSourceMVector :: IO ()
testSourceMVector = testSourceMVector' [1, 2, 6, 3]

testSourceMVectorEmpty :: IO ()
testSourceMVectorEmpty = testSourceMVector' []

testConsumeVector' :: V.Vector Int -> IO ()
testConsumeVector' v = do let l = V.toList v
                          v' <- CL.sourceList l $$ consumeVector
                          assertEqual "consumed vector" v v'

testConsumeVector :: IO ()
testConsumeVector = testConsumeVector' (V.fromList [1, 2, 6, 3])

testConsumeVectorEmpty :: IO ()
testConsumeVectorEmpty = testConsumeVector' V.empty

testConsumeMVector' :: M.MVector (PrimState IO) Int -> IO ()
testConsumeMVector' v = do i <- V.freeze v
                           let l = V.toList i
                           v' <- CL.sourceList l $$ consumeMVector
                           i' <- V.unsafeFreeze v'
                           assertEqual "consumed vector" i i'

testConsumeMVector :: IO ()
testConsumeMVector = do m <- V.unsafeThaw . V.fromList $ [1, 2, 6, 3]
                        testConsumeMVector' m

testConsumeMVectorEmpty :: IO ()
testConsumeMVectorEmpty = do m <- V.unsafeThaw V.empty
                             testConsumeMVector' m

tests :: [F.Test]
tests = [F.testGroup "Properties"
            [testProperty "Inverse" testInverse,
             testProperty "Inverse2" testInverse2,
             testProperty "Inverse3" testInverse
             ],
         F.testGroup "Cases" [
             testCase "sourceVector" testSourceVector,
             testCase "sourceVector (empty)" testSourceVectorEmpty,
             testCase "sourceMVector" testSourceMVector,
             testCase "sourceMVector (empty)" testSourceMVectorEmpty,
             testCase "consumeVector" testConsumeVector,
             testCase "consumeVector (empty)" testConsumeVectorEmpty,
             testCase "consumeMVector" testConsumeMVector,
             testCase "consumeMVector (empty)" testConsumeMVectorEmpty
             ]
         ]

main :: IO ()
main = defaultMain tests
