module Scoreboard where

import System.IO

data Key = Exit | ResetBoard

run :: IO()
run = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  putStrLn $ "SCOREBOARD started."
  loop getContents
  putStrLn $ "SCOREBOARD stopped."
  return ()

loop :: IO [Char] -> IO ()
loop ioChars = do
  chars <- ioChars
  let displays = process $ toKeys chars
  consume displays

consume :: [String] -> IO()
consume [] = return ()
consume (aMessage : rest) = do
  putStrLn aMessage
  consume rest

toKeys :: [Char] -> [Key]
toKeys ('x' : rest) = (Exit : toKeys rest)
toKeys ('r' : rest) = (ResetBoard : toKeys rest)
toKeys (unknown : rest) = toKeys rest

process :: [Key] -> [String]
process (Exit : _) = []
process (ResetBoard : rest) = ("Resetting " : process rest)
