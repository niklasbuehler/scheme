module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Char (digitToInt)
import Data.List (foldl')

main :: IO ()
main = do (expr:_) <- getArgs
          putStrLn (readExpr expr)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char
             | Bool Bool
             deriving Show

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escape <|> (noneOf "\""))
                 char '"'
                 return $ String x
  where escape :: Parser Char
        escape = do x <- char '\\' >> oneOf "\"nrt\\"
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

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                 "#t" -> Bool True
                 "#f" -> Bool False
                 _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do num <- parseDecimal
                    <|> parseDecimal'
                    <|> parseBinary
                    <|> parseOctal
                    <|> parseHex
                 return $ num
  where parseDecimal = liftM (Number . read) $ many1 digit
        parseDecimal' = do try $ string "#d"
                           x <- many1 digit
                           return $ Number (read x)
        parseBinary = do try $ string "#b"
                         x <- many1 (oneOf "01")
                         return $ Number (readBin x)
        readBin = foldl' (\acc x -> acc * 2 + (toInteger . digitToInt) x) 0
        parseOctal = do try $ string "#o"
                        x <- many1 octDigit
                        return $ Number (fst (readOct x !! 0))
        parseHex = do try $ string "#x"
                      x <- many1 hexDigit
                      return $ Number (fst (readHex x !! 0))

parseExpr :: Parser LispVal
parseExpr = parseCharacter
        <|> parseString
        <|> parseNumber
        <|> parseAtom
