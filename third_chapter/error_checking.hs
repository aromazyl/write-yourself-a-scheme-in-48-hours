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
import Numeric
import Data.Array
import Data.Ratio
import Data.Complex
import Control.Monad.Error

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>=
    eval
  putStrLn $ extractValue $ trapError evaled

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> throwError $ Parser err
                   Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Float
             | Ratio Rational
             | Complex (Complex Double)
             | Vector (Array Int LispVal)

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
                  x <- many $ (escapedChars <|> noneOf "\"\\")
                  char '"'
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

parseNumber3 :: Parser LispVal
parseNumber3 = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= (return . Number . read)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do try $ string "#d"
                   x <- many1 digit
                   (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in bin2dig' old xs

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> try parseNumber
        <|> try parseString1
        <|> try parseBool
        <|> try parseCharacter
        <|> try parseFloat
        <|> try parseRatio
        <|> parseQuoted
        <|> do char '('
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x
        <|> parseQuasiQuoted
        <|> parseQuasiQuoted
        <|> try (do string "#("
                    x <- parseVector
                    char ')'
                    return x)

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 'f' >> return (Bool False)) <|> (char 't' >> return (Bool True))

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  value <- try (string "newline" <|> string "space")
          <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
  return $ case value of
             "space" -> Character ' '
             "newline" -> Character '\n'
             otherwise -> Character (value !! 0)

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst . head $ readFloat (x ++ "." ++ y))

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))

toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do x <- try $ parseFloat <|> parseDecimal1
                  char '+'
                  y <- try $ parseFloat <|> parseDecimal1
                  char 'i'
                  return $ Complex (toDouble x :+ toDouble y)

parseList ::  Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

{-
parseAnyList :: Parser LispVal
parseAnyList = do
  P.char '('
  optionalSpaces
  head <- P.sepEndBy parseExpr spaces
  tail <- (P.char '.' >> spaces >> parseExpr) <|> return (Nil ())
  optionalSpaces
  P.char ')'
  return $ case tail of
             (Nil ()) -> List head
             otherwise -> DottedList head tail
-}

parseList1 :: Parser LispVal
parseList1 = between beg end parseList2
           where beg = (char '(' >> skipMany space)
                 end = (skipMany space >> char ')')

parseList2 :: Parser LispVal
parseList2 = do list <- sepEndBy parseExpr spaces
                maybeDatum <- optionMaybe (char '.' >> spaces >> parseExpr)
                return $ case maybeDatum of
                           Nothing -> List list
                           Just datum -> DottedList list datum

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseVector :: Parser LispVal
parseVector = do arrayValues <- sepBy parseExpr spaces
                 return $ Vector (listArray (0, (length arrayValues - 1)) arrayValues)

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character x) = show x
showVal (Float num) = show num
showVal (Ratio ratio) = show ratio
showVal (Complex num) = show num
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitives function args" func) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp $ return . isSymbol),
              ("string?", unaryOp $ return . isString),
              ("number?", unaryOp $ return . isNumber),
              ("bool?", unaryOp $ return . isBool),
              ("list?", unaryOp $ return . isList)]

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [v] = op v

isSymbol, isString, isNumber, isBool, isList :: LispVal -> LispVal
isSymbol (Atom _)   = Bool True
isSymbol _          = Bool False
isString (String _) = Bool True
isString _          = Bool False
isNumber (Number _) = Bool True
isNumber _          = Bool False
isBool   (Bool _)   = Bool True
isBool   _          = Bool False
isList   (List _)   = Bool True
isList   _          = Bool False


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum notNum = throwError $ TypeMismatch "number" notNum
unpackNum (List [n]) = unpackNum n

unpackNum2 :: LispVal -> Integer
unpackNum2 (Number n) = n
unpackNum2 _ = 0

symbol2string, string2symbol :: LispVal -> LispVal
symbol2String (Atom s)    = String s
symbol2string _           = String ""
string2symbol (String s)  = Atom s
string2symbol _           = Atom ""

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args: found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
instance Show LispError where show = showError

instance Error LispError where
  noMsg = Default "An error is occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
