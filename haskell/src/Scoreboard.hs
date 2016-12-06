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
process scoreboard (key : rest) =
  message command scoreboard nextScoreboard ++ process nextScoreboard rest where
    command = getCommand key
    nextScoreboard = operation command scoreboard

type Operation = Scoreboard -> Scoreboard
type Message = Scoreboard -> Scoreboard -> [String]
data Command = Command { operation :: Operation, message :: Message }

getCommand :: Key -> Command
getCommand ResetBoard = Command {
  operation = \scoreboard -> newScoreboard,
  message = \oldScorebaord newScoreboard -> ["Score set to " ++ currentScore newScoreboard]
}
getCommand SelectA = createSelectCommand TeamA
getCommand SelectB = createSelectCommand TeamB
getCommand Score1 = createScoreCommand 1
getCommand Score2 = createScoreCommand 2
getCommand Score3 = createScoreCommand 3

createSelectCommand selection = Command {
  operation = \scoreboard -> selectTeam scoreboard selection,
  message = \oldScorebaord newScoreboard -> [formatSelection newScoreboard]
}

createScoreCommand points = Command {
  operation = \scoreboard -> case scoreboard of
      Scoreboard _ _ None -> scoreboard
      _                   -> score scoreboard points,
  message = \oldScorebaord newScoreboard -> case oldScorebaord of
    Scoreboard _ _ None -> []
    _                   -> [currentScore newScoreboard]
}

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
