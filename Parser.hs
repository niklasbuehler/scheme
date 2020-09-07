module Parser where
import Numeric
import Data.Char (digitToInt)
import Data.Ratio
import Data.Complex
import Data.Array
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.List (foldl')
import Control.Monad

import Base

--- Parsing ---
parseScheme :: String -> Either ParseError LispVal
parseScheme input = parse parseExpr "lisp" input

parseExpr :: Parser LispVal
parseExpr = parseCharacter
        <|> parseString
        <|> try parseReal
        <|> try parseRational
        <|> try parseComplex
        <|> parseNumber
        <|> parseQuoted
        <|> parseQuasiQuoted
        <|> try parseUnQuote
        <|> parseUnQuoteSplicing
        <|> parseAtom
        <|> parseVector
        <|> parseBool
        <|> parseListType

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "?!$%^&+-*/:<=>_~|"

--- Parsing Character Types ---
parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (parseEscapedCharacter <|> (noneOf "\""))
                 char '"'
                 return $ String x

parseEscapedCharacter :: Parser Char
parseEscapedCharacter = do try $ char '\\'
                           x <- oneOf "\"nrt\\"
                           return $ case x of
                             'n' -> '\n'
                             'r' -> '\r'
                             't' -> '\t'
                             _   -> x

parseCharacter :: Parser LispVal
parseCharacter = do try $ string "#\\"
                    x <- try (string "newline" <|> string "space")
                         <|> do {x <- anyChar; notFollowedBy alphaNum; return [x]}
                    return $ Character $ case x of
                      "space"   -> ' '
                      "newline" -> '\n'
                      otherwise -> (x !! 0)

--- Parsing Lists ---
parseListType :: Parser LispVal
parseListType = do char '('
                   x <- try parseList <|> parseDottedList
                   char ')'
                   return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do char '`'
                      x <- parseExpr
                      return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do char ','
                  x <- parseExpr
                  return $ List [Atom "unquote", x]

parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = do try $ string ",@"
                          x <- parseExpr
                          return $ List [Atom "unquote-splicing", x]

--- Parsing Vectors ---
parseVector :: Parser LispVal
parseVector = do try $ string "#("
                 vecVals <- sepBy parseExpr spaces
                 char ')'
                 return $ Vector (listArray (0, (length vecVals - 1)) vecVals)

--- Parsing Atoms ---
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ Atom atom

--- Parsing Booleans ---
parseBool :: Parser LispVal
parseBool = do try $ char '#'
               val <- oneOf "tf"
               return $ case val of
                 't' -> Bool True
                 'f' -> Bool False

--- Parsing Numerical Types ---
parseNumber :: Parser LispVal
parseNumber = do num <- parseDecimal
                    <|> parseDecimal'
                    <|> parseBinary
                    <|> parseOctal
                    <|> parseHex
                 return $ num

parseDecimal :: Parser LispVal
parseDecimal = liftM (Number . read) $ many1 digit

parseDecimal' :: Parser LispVal
parseDecimal' = do try $ string "#d"
                   x <- many1 digit
                   return $ Number (read x)

parseBinary :: Parser LispVal
parseBinary = do try $ string "#b"
                 x <- many1 (oneOf "01")
                 return $ Number (readBin x)
  where readBin :: String -> Integer
        readBin = foldl' (\acc x -> acc * 2 + (toInteger . digitToInt) x) 0

parseOctal :: Parser LispVal
parseOctal = do try $ string "#o"
                x <- many1 octDigit
                return $ Number (fst (readOct x !! 0))

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number (fst (readHex x !! 0))

parseReal :: Parser LispVal
parseReal = do x <- many1 digit
               char '.'
               y <- many1 digit
               return $ Real (fst.head $ readFloat (x ++ "." ++ y))

parseRational :: Parser LispVal
parseRational = do x <- many1 digit
                   char '/'
                   y <- many1 digit
                   return $ Rational ((read x) % (read y))

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseReal <|> parseDecimal)
                  char '+'
                  y <- (try parseReal <|> parseDecimal)
                  char 'i'
                  return $ Complex (toDouble x :+ toDouble y)
  where toDouble :: LispVal -> Double
        toDouble (Real r) = realToFrac r
        toDouble (Number n) = fromIntegral n
