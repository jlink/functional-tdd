module ScoreboardAppSpec (spec) where

import           Scoreboard
import           ScoreboardApp
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "ScoreboardApp.process" $ do
    it "initial score is 000:000" $ do
      process newScoreboard [] `shouldBe` ["000:000"]

  describe "ScoreboardApp.formatScore" $ do
    it "single digit scores are filled in with zeros" $ do
      formatScore (1, 9) `shouldBe` "001:009"
    it "multi digit scores are filled in if necessary" $ do
      formatScore (11, 999) `shouldBe` "011:999"
    it "more than 3 digits are left alone" $ do
      formatScore (1234, 98765) `shouldBe` "1234:98765"

  describe "ScoreboardApp.toCommands" $ do
    it "lines are converted to commands" $ do
      toCommands ["a", "b", "+", "-", "r", "x"] `shouldBe`
        [SelectA, SelectB, Increment, Decrement, ResetBoard, Exit]
    it "lines are sanitized before conversion" $ do
      toCommands ["   a   ", "B"] `shouldBe` [SelectA, SelectB]
    it "unknown commands are skipped" $ do
      toCommands ["a", "z", "ab", "x"] `shouldBe` [SelectA, Exit]

  describe "ScoreboardApp.processCommands" $ do
    it "commands are processed in order" $ do
      let messages = processCommands newScoreboard [SelectA, Increment]
      messages `shouldBe` ["Team A selected", "001:000"]
    it "Exit ends all command processing" $ do
      let messages = processCommands newScoreboard [SelectA, Exit, Increment]
      messages `shouldBe` ["Team A selected"]

  describe "ScoreboardApp.actions" $ do
    it "action for ResetBoard will reset scoreboard" $ do
      let action = getAction ResetBoard
      let oldScoreboard = (Scoreboard (1, 2) TeamA)
      let newScoreboard = operation action oldScoreboard
      newScoreboard `shouldBe` (Scoreboard (0, 0) None)
      message action oldScoreboard newScoreboard `shouldBe` "000:000"
