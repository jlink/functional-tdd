module ScoreboardSpec (spec) where

import           Scoreboard
import           ScoreboardApp
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "ScoreboardApp.process" $ do
    it "initial score is 000:000" $ do
      process newScoreboard [] `shouldBe` ["000:000"]
    it "initial score is 000:000" $ do
      process newScoreboard [] `shouldBe` ["000:000"]


  describe "Scoreboard" $ do
    it "current score of newScoreboard is 0 : 0" $ do
      let sb = newScoreboard
      currentScore sb `shouldBe` (0, 0)
