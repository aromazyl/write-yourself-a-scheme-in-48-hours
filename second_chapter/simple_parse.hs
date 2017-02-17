#! /usr/bin/env runhugs +l
--
-- simple_parse.hs
-- Copyright (C) 2017 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--
{-# LANGUAGE Arrows #-}

module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do args <- getArgs
          (return $ readExpr $ args !! 0) >>= putStrLn

symbol :: Parser Char
symbol = oneOf "!$%&*+_/:<=?>@^_~#"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
      Left err -> "No match: " ++ (show err :: String)
      Right val -> "Found value"
