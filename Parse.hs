module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Ratio
import Data.Complex

main :: IO ()
main = do (expr:_) <- getArgs
          putStrLn (readExpr expr)

--- Datatypes ---
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Real Double
             | Rational Rational
             | Complex (Complex Double)
             | String String
             | Character Char
             | Bool Bool
             deriving Show

--- Parsing ---
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

parseExpr :: Parser LispVal
parseExpr = parseCharacter
        <|> parseString
        <|> try parseComplex
        <|> try parseRational
        <|> try parseReal
        <|> parseNumber
        <|> parseAtom

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

--- Parsing Character Types ---
parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapedChar <|> (noneOf "\""))
                 char '"'
                 return $ String x
  where escapedChar :: Parser Char
        escapedChar = do char '\\'
                         x <- oneOf "\"nrt\\"
                         return $ case x of
                           'n' -> '\n'
                           'r' -> '\n'
                           't' -> '\n'
                           _   -> x

parseCharacter :: Parser LispVal
parseCharacter = do try $ string "#\\"
                    x <- try (string "newline" <|> string "space")
                         <|> do {x <- anyChar; notFollowedBy alphaNum; return [x]}
                    return $ Character $ case x of
                      "space"   -> ' '
                      "newline" -> '\n'
                      otherwise -> (x !! 0)

--- Parsing Atoms ---
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                 "#t" -> Bool True
                 "#f" -> Bool False
                 _    -> Atom atom

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
