module Main where

import Syntax
import Scanner
import Evaluation

import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline
        
process :: String -> IO ()
process line = do
  let result = parseExpr line
  case result of
    Left error -> print error
    Right exp -> do
      let output = eval exp []
      print output

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "8) "
    case minput of
      Nothing -> outputStrLn "8("
      Just input -> do
          let first = head $ words $ input
          case first of
              ":load" -> (liftIO $ print "load command :D") >> loop
              _ -> (liftIO $ process input) >> loop
