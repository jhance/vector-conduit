module Main (main)
where

import Control.Monad.ST
import Data.Conduit
import qualified Data.Vector as V
import Data.Vector.Conduit
import Test.QuickCheck
import qualified Test.Framework as F
import Test.Framework.Runners.Console (defaultMain)
import Test.Framework.Providers.QuickCheck2

testInverse :: [Int] -> Bool
testInverse l = runST $ do let v = V.fromList l
                           v' <- runResourceT $ sourceVector v $$ consumeVector
                           return $ v == v'

testInverse2 :: [Int] -> Bool
testInverse2 l = runST $ do let v = V.fromList l
                            v' <- runResourceT $ sourceVector v $$ consumeMVector
                            v'' <- V.unsafeFreeze v'
                            return $ v == v''

testInverse3 :: [[Int]]-> Bool
testInverse3 ls = runST $ do let vs = map V.fromList ls
                             let v = V.fromList vs
                             v' <- runResourceT $ sourceVector v $= thawConduit $= freezeConduit $$ consumeVector
                             return $ v == v'

tests :: [F.Test]
tests = [testProperty "Inverse" testInverse,
         testProperty "Inverse2" testInverse2,
         testProperty "Inverse3" testInverse3]

main :: IO ()
main = defaultMain tests
