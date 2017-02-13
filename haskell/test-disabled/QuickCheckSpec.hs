module QuickCheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property
import Control.Exception (evaluate)
import IoSample

spec :: Spec
spec = do
  describe "Using QuickCheck" $ do
    it "with external property" $ property $
      prop_revapp
    it "with inlined property" $ property $
      \x -> x + 1 > (x :: Int)
    it "with a counterexample" $ property $
      prop_three

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (ys++xs) == reverse xs ++ reverse ys

prop_three :: Integer -> Property
prop_three n = counterexample ("always fails: n = " ++ show n) False
