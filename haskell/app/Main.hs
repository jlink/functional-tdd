module Main where

import Scoreboard

main :: IO ()
main = Scoreboard.run

-- import System.IO
-- import System.IO.Unsafe
-- main = do
--   hSetBuffering stdin NoBuffering
--   hSetEcho stdin False
--   chars <- myGetContents
--   -- chars <- getContents
--   consume chars
--
-- consume :: [Char] -> IO ()
-- consume [] = return ()
-- consume ('x':_) = consume []
-- consume (c : rest) = do
--   putStrLn $ "IN: " ++ [c]
--   consume rest
--
-- myGetContents :: IO [Char]
-- myGetContents = do
--   c <- getChar
--   rest <- unsafeInterleaveIO myGetContents
--   return (c : rest)
