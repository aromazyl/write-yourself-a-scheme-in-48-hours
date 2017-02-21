#! /usr/bin/env runhugs +l
--
-- repl.hs
-- Copyright (C) 2017 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--

module Main where


import Control.Monad
import SimpleParser
import System.Environment
import System.IO hiding (try)

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt s = flushStr s >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ (liftM show) $ readExpr expr >>= eval

evalAndPrint :: String -> IO ()
evalAndPrint s = evalString s >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>>") evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> evalAndPrint $ args !! 0
    otherwise -> putStrLn "Programs take only 0 or 1 argument"
