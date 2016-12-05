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
  let messages = process $ toKeys chars
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

process :: [Key] -> [String]
process (Exit : _) = []
process (key : rest) = (("Key: " ++ show(key)) : process rest)
