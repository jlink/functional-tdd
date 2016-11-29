import Test.QuickCheck
import Test.QuickCheck.Property

main = do
  quickCheck prop_revapp
  quickCheck prop_revapp2
  quickCheck prop_three

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (ys++xs) == reverse xs ++ reverse ys

prop_revapp2 :: [Int] -> [Int] -> Bool
prop_revapp2 xs ys = reverse (xs++ys) == reverse xs ++ reverse ys

prop_three :: Integer -> Property
prop_three n = counterexample ("always fails: n = " ++ show n) False
