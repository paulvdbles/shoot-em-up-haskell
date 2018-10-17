import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC

import           Data.List
import           Data.Ord

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps =
  testGroup
    "(checked by SmallCheck)"
    [ SC.testProperty "sort == sort . reverse" $ \list -> sort (list :: [Int]) == sort (reverse list)
    , SC.testProperty "Fermat's little theorem" $ \x -> ((x :: Integer) ^ 7 - x) `mod` 7 == 0
  -- the following property does not hold
    , SC.testProperty "Fermat's last theorem" $ \x y z n ->
        (n :: Integer) >= 3 SC.==> x ^ n + y ^ n /= (z ^ n :: Integer)
    ]

qcProps =
  testGroup
    "(checked by QuickCheck)"
    [ QC.testProperty "sort == sort . reverse" $ \list -> sort (list :: [Int]) == sort (reverse list)
    , QC.testProperty "Fermat's little theorem" $ \x -> ((x :: Integer) ^ 7 - x) `mod` 7 == 0
  -- the following property does not hold
    , QC.testProperty "Fermat's last theorem" $ \x y z n ->
        (n :: Integer) >= 3 QC.==> x ^ n + y ^ n /= (z ^ n :: Integer)
    ]

unitTests =
  testGroup
    "Unit tests"
    [ testCase "List comparison (different length)" $ [1, 2, 3] `compare` [1, 2] @?= GT
  -- the following test does not hold
    , testCase "List comparison (same length)" $ [1, 2, 3] `compare` [1, 2, 2] @?= LT
    ]

--{-# LANGUAGE TemplateHaskell #-}
--module QuickCheck where
--
--{- IMPORTS -}
--import TestsCode
--
--import Test.HUnit
--import Test.QuickCheck
--import Test.QuickCheck.All
--
--import Data.List
--import Data.Char
--
--import System.IO
--import System.Exit
--
--{- GENERATORS -}
--
--
--{- TESTS -}
--
--
--{- THIS CODE EXISTS TO AUTO RUN ALL prop_ TESTS -}
--return [] -- this prevents GHC from showing some warnings
--runTests :: IO Bool
--runTests = $forAllProperties $
--  quickCheckWithResult (stdArgs {maxSuccess = 1000})
--
--main :: IO ()
--main = do
--  -- add test runners into the array for each module
--  good <- and <$> sequence [runTests {-, MyModule.runTests, etc-}]
--  if good
--     then exitSuccess
--     else exitFailure
