module Scoreboard where

import System.IO

data Key = Exit | ResetBoard | SelectA | SelectB | Increment | Decrement
  deriving (Show)

run :: IO()
run = do
  putStrLn $ "SCOREBOARD started."
  loop getContents putStrLn
  putStrLn $ "SCOREBOARD stopped."
  return ()

loop :: IO String -> (String -> IO ()) -> IO ()
loop contentsReader printer = do
  contents <- contentsReader
  let commandLines = lines contents
  let messages = process newScoreboard $ toKeys commandLines
  mapM_ printer (currentScore newScoreboard : messages)
  -- mapM_ printer ["OOPS"]

toKeys :: [String] -> [Key]
toKeys ("x" : rest) = (Exit : toKeys rest)
toKeys ("c" : rest) = (ResetBoard : toKeys rest)
toKeys ("a" : rest) = (SelectA : toKeys rest)
toKeys ("b" : rest) = (SelectB : toKeys rest)
toKeys ("+" : rest) = (Increment : toKeys rest)
toKeys ("-" : rest) = (Decrement : toKeys rest)
toKeys (unknown : rest) = toKeys rest

process :: Scoreboard -> [Key] -> [String]
process scoreboard (Exit : _) = []
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
  message = \oldScorebaord newScoreboard -> [currentScore newScoreboard]
}
getCommand SelectA = createSelectCommand TeamA
getCommand SelectB = createSelectCommand TeamB
getCommand Increment = createScoreCommand 1
getCommand Decrement = createScoreCommand (-1)

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
score (Scoreboard a b TeamA) s = Scoreboard (a + s) b TeamA
score (Scoreboard a b TeamB) s = Scoreboard a (b + s) TeamB

formatSelection :: Scoreboard -> String
formatSelection (Scoreboard _ _ TeamA) = "Team A selected"
formatSelection (Scoreboard _ _ TeamB) = "Team B selected"

leftPad0 :: Int -> String -> String
leftPad0 n s
    | length s < n  = replicate (n - length s) '0' ++ s
    | otherwise = s
