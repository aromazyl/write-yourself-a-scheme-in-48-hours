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
import Control.Monad

main :: IO ()
main = do args <- getArgs
          (return $ readExpr $ args !! 0) >>= putStrLn

symbol :: Parser Char
symbol = oneOf "!$%&*+_/:<=?>@^_~#"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
      Left err -> "No match: " ++ (show err :: String)
      Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
           | List [LispVal]
           | DottedList [LispVal]
           | Number Integer
           | String String
           | Bool Bool

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\"ntr"
                  return $ case x of
                    'n'  -> '\n'
                    't'  -> '\t'
                    'r'  -> '\r'
                    '\\' -> x
                    '"'  -> x

parseString1 :: Parser LispVal
parseString1 = do char '"'
                  x <- many $ escapedChars <|> noneOf "\"\\"
                  return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseNumber1 :: Parser LispVal
parseNumber1 = do nu <- (many1 digit)
                  return $ (Number . read) nu

parseNumber2 :: Parser LispVal
parseNumber2 = (many1 digit)
           >>= (\x -> (return $ (Number . read) x))

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseNumber
        <|> parseString1
