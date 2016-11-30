import Test.Hspec

import qualified QuickCheckSpec
import qualified IoSampleSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "QuickCheck" QuickCheckSpec.spec
  describe "IoSampleSpec" IoSampleSpec.spec
