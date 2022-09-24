module Main where

import Syntax
import Scanner
import Evaluation
import Check

import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline
        
repl :: String -> IO ()
repl line = do
  let ast = parseExpr line
  case ast of
    Left error -> print error
    Right expr -> do
      let tc = checkTop [] expr
      case tc of
        Left tError -> print tError
        Right _ -> print $ eval expr []

run :: String -> IO ()
run filename = do
    s <- readFile filename
    let ast = parseExpr s
    case ast of
        Left error -> print error
        Right expr -> do
            let output = eval expr []
            print output

type' :: String -> IO ()
type' expr = do
  let ast = parseExpr expr
  case ast of
    Left error -> print error
    Right expr' -> do
      let res = checkTop [] expr'
      print res

tlc :: IO ()
tlc = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "\\.> "
    case minput of
      Just input -> do
          let first = head $ words input
          case first of
              ":run"  -> (liftIO $ run ((words $ input)!!1)) >> loop
              ":type" -> (liftIO $ type' (unwords $ drop 1 $ words input)) >> loop
              ":exit" -> outputStrLn "quitting repl..."
              _ -> (liftIO $ repl input) >> loop

main :: IO ()
main = do
  tlc