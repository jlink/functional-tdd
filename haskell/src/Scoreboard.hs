module Scoreboard where

import System.IO

run = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  putStrLn $ "SCOREBOARD started."
  loop
  putStrLn $ "SCOREBOARD stopped."
  return ()

loop :: IO ()
loop = do
  c <- getChar
  if c == 'x'
    then return ()
    else do
      putStrLn $ "Input: " ++ [c]
      loop
