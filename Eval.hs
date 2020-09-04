module Main where
import System.Environment
import Parse

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

readExpr :: String -> LispVal
readExpr input = case parseScheme input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

eval :: LispVal -> LispVal
eval val@(Character _) = val
eval val@(String _) = val
eval val@(Real _) = val
eval val@(Rational _) = val
eval val@(Complex _) = val
eval val@(Number _) = val
eval val@(Atom _) = val
eval val@(Vector _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval val@(List _) = val
eval val@(DottedList _ _) = val

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp isSymbol),
              ("char?", unaryOp isCharacter),
              ("string?", unaryOp isString),
              ("number?", unaryOp isNumber),
              ("complex?", unaryOp isComplex),
              ("real?", unaryOp isReal),
              ("rational?", unaryOp isRational),
              ("bool?", unaryOp isBool),
              ("list?", unaryOp isList),
              ("pair?", unaryOp isPair),
              ("vector?", unaryOp isVector)]

isSymbol, isCharacter, isString, isNumber, isReal, isRational, isComplex, isBool, isList, isPair, isVector :: LispVal -> LispVal
isSymbol (Atom _)         = Bool True
isSymbol _                = Bool False
isCharacter (Character _) = Bool True
isCharacter _             = Bool False
isString (String _)       = Bool True
isString _                = Bool False
isNumber (Number _)       = Bool True
isNumber _                = Bool False
isReal (Real _)           = Bool True
isReal _                  = Bool False
isRational (Rational _)   = Bool True
isRational _              = Bool False
isComplex (Complex _)     = Bool True
isComplex _               = Bool False
isBool (Bool _)           = Bool True
isBool _                  = Bool False
isList (List _)           = Bool True
isList _                  = Bool False
isPair (DottedList _ _)   = Bool True
isPair _                  = Bool False
isVector (Vector _)       = Bool True
isVector _                = Bool False

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0
