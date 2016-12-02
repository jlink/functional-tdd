module Scoreboard where

import System.IO

data Key = Exit | OtherKey Char

run :: IO()
run = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  putStrLn $ "SCOREBOARD started."
  loop getContents
  -- loop readChars
  putStrLn $ "SCOREBOARD stopped."
  return ()

readChars :: IO [Char]
-- Type is correct, but result seems to be never delivered
readChars = do
  c <- getChar
  readChars >>= \cs -> return (c : cs)

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
toKeys (c : rest) =
  if c == 'x'
    then (Exit : toKeys rest)
    else (OtherKey c : toKeys rest)

process :: [Key] -> [String]
process (Exit : _) = []
process (OtherKey c : rest) = (("Unhandled Input: " ++ [c]) : process rest)
