module Scoreboard where

import System.IO

data Key = Exit | ResetBoard

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
toKeys (unknown : rest) = toKeys rest

process :: [Key] -> [String]
process (Exit : _) = []
process (ResetBoard : rest) = ("Resetting " : process rest)
