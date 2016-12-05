module Scoreboard where

import System.IO

data Key = Exit | ResetBoard | SelectA | SelectB | Score1 | Score2 | Score3
  deriving (Show)

run :: IO()
run = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  putStrLn $ "SCOREBOARD started."
  loop getContents putStrLn
  putStrLn $ "SCOREBOARD stopped."
  return ()

loop :: IO [Char] -> (String -> IO ()) -> IO ()
loop charReader printer = do
  chars <- charReader
  let messages = process newScoreboard $ toKeys chars
  mapM_ printer messages

toKeys :: [Char] -> [Key]
toKeys ('x' : rest) = (Exit : toKeys rest)
toKeys ('r' : rest) = (ResetBoard : toKeys rest)
toKeys ('a' : rest) = (SelectA : toKeys rest)
toKeys ('b' : rest) = (SelectB : toKeys rest)
toKeys ('1' : rest) = (Score1 : toKeys rest)
toKeys ('2' : rest) = (Score2 : toKeys rest)
toKeys ('3' : rest) = (Score3 : toKeys rest)
toKeys (unknown : rest) = toKeys rest

process :: Scoreboard -> [Key] -> [String]
process scoreboard (Exit : _) = ["Final Score is " ++ currentScore scoreboard]
process scoreboard (ResetBoard : rest) =
  (("Score set to " ++ (currentScore nextScoreboard)) : process nextScoreboard rest) where
    nextScoreboard = newScoreboard
process scoreboard (SelectA : rest) =
  ((formatSelection nextScoreboard) : process nextScoreboard rest) where
    nextScoreboard = selectTeam scoreboard TeamA
process scoreboard (SelectB : rest) =
  ((formatSelection nextScoreboard) : process nextScoreboard rest) where
    nextScoreboard = selectTeam scoreboard TeamB
process (Scoreboard a b None) (_ : rest) = process (Scoreboard a b None) rest
process scoreboard (Score1 : rest) =
  ((currentScore nextScoreboard) : process nextScoreboard rest) where
    nextScoreboard = score scoreboard 1
process scoreboard (Score2 : rest) =
  ((currentScore nextScoreboard) : process nextScoreboard rest) where
    nextScoreboard = score scoreboard 2
process scoreboard (Score3 : rest) =
  ((currentScore nextScoreboard) : process nextScoreboard rest) where
    nextScoreboard = score scoreboard 3

-- processStep :: Scoreboard -> [Key] -> (Scoreboard -> Scoreboard) -> (Scoreboard -> String) -> [String]
-- processStep scoreboard remainingKeys nextStep describe =
--   ((describe nextScoreboard) : process nextScoreboard remainingKeys) where
--     nextScoreboard = nextStep scoreboard

type Score = Int
data Selection = TeamA | TeamB | None
data Scoreboard = Scoreboard Score Score Selection

newScoreboard :: Scoreboard
newScoreboard = Scoreboard 0 0 None

currentScore :: Scoreboard -> String
currentScore (Scoreboard a b _) = (formatScore a) ++ ":" ++ (formatScore b)

formatScore :: Score -> String
formatScore s = leftPad0 3 (show s)

selectTeam :: Scoreboard -> Selection -> Scoreboard
selectTeam (Scoreboard a b _) s = Scoreboard a b s

score :: Scoreboard -> Score -> Scoreboard
score (Scoreboard a b None) _ = Scoreboard a b None
score (Scoreboard a b TeamA) s = Scoreboard (a + s) b None
score (Scoreboard a b TeamB) s = Scoreboard a (b + s) None

formatSelection :: Scoreboard -> String
formatSelection (Scoreboard _ _ None) = "No team selected"
formatSelection (Scoreboard _ _ TeamA) = "Team A selected"
formatSelection (Scoreboard _ _ TeamB) = "Team B selected"

leftPad0 :: Int -> String -> String
leftPad0 n s
    | length s < n  = replicate (n - length s) '0' ++ s
    | otherwise = s
