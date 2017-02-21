#! /usr/bin/env runhugs +l
--
-- simple_parse.hs
-- Copyright (C) 2017 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Array
import Data.Ratio
import Data.Complex
import Data.Boolean
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
eval (List ((Atom "cond") : cs)) = do
  b <- (liftM (take 1 . dropWhile f) $ mapM condClause cs) >>= cdr
  car [b] >>= eval
    where condClause (List [p, b]) = do q <- eval p
                                        case q of
                                          Bool _ -> return $ List [q, b]
                                          _      -> throwError $ TypeMismatch "bool" q
          condClause v             = throwError $ TypeMismatch  "(pred body)" v
          f                        = \(List [a, b]) -> case a of
                                                      (Bool False) -> True
                                                      _            -> False

eval (List [Atom "if", pred , conseq, alt]) =
                     do result <- eval pred
                        case result of
                          Bool False -> eval alt
                          otherwise -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

eval form@(List (Atom "case" : key : clause)) =
  if null clause
  then throwError $ BadSpecialForm "no true clause in case expression: " form
  else case head clause of
    List (Atom "else" : exprs) -> mapM eval exprs >>= return . last
    List ((List datums) : exprs) -> do
      result <- eval key
      equality <- mapM (\x -> eqv [x, result]) datums
      if (Bool True) `elem` equality
      then mapM eval exprs >>= return . last
      else eval $ List (Atom "case" : key : tail clause)
    _ -> throwError $ BadSpecialForm "ill-formed case expression: " form

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitives function args" func) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=",numBoolBinop (/=)),
              (">=",numBoolBinop (>=)),
              ("<=",numBoolBinop (<=)),
              ("&&",boolBoolBinop (&&)),
              ("||",boolBoolBinop (||)),
              ("mod", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp $ return . isSymbol),
              ("string?", unaryOp $ return . isString),
              ("string=?", stringBoolBinop (==)),
              ("string<?", stringBoolBinop (<)),
              ("string<=?", stringBoolBinop (<=)),
              ("string>=?", stringBoolBinop (>=)),
              ("number?", unaryOp $ return . isNumber),
              ("bool?", unaryOp $ return . isBool),
              ("list?", unaryOp $ return . isList),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eqv?", eqv),
              ("equal?", equal)]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do
                               left <- unpacker $ args !! 0
                               right <- unpacker $ args !! 1
                               return $ Bool $ left `op` right


numBoolBinop = boolBinop unpackNum
stringBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

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
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

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

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [DottedList [xs] x] = return x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs xlast] = return $ DottedList ([x] ++ xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons [badArg] = throwError $ TypeMismatch "pair" badArg
cons badArgList = throwError $ NumArgs 1 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (and $ map eqvPair $ zip arg1 arg2)
  where eqvPair (v1, v2) = case eqv [v1, v2] of
                             Left err -> False
                             Right (Bool val) -> val

eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [_, _] = return $ Bool False
eqv [badArg] = throwError $ TypeMismatch "pair" badArg
eqv badArgList = throwError $ NumArgs 1 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
      [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)

equal badArgList = throwError $ NumArgs 2 badArgList


