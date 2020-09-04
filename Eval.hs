module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Parse
import Control.Monad.Except

main :: IO ()
main = do
          args <- getArgs
          evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
          putStrLn $ extractValue $ trapError evaled

--- Evaluating ---
readExpr :: String -> ThrowsError LispVal
readExpr input = case parseScheme input of
  Left err  -> throwError $ Parser err
  Right val -> return val

eval :: LispVal -> ThrowsError LispVal
eval val@(Character _)          = return val
eval val@(String _)             = return val
eval val@(Real _)               = return val
eval val@(Rational _)           = return val
eval val@(Complex _)            = return val
eval val@(Number _)             = return val
eval val@(Atom _)               = return val
eval val@(Vector _)             = return val
eval val@(Bool _)               = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args))  = mapM eval args >>= apply func
eval val@(List _)               = return val
eval val@(DottedList _ _)       = return val
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

--- Primitives ---
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol->string", unaryOp symbolToString),
              ("string->symbol", unaryOp stringToSymbol),
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

symbolToString, stringToSymbol :: LispVal -> LispVal
symbolToString (Atom s)   = String s
symbolToString _          = String ""
stringToSymbol (String s) = Atom s
stringToSymbol _          = Atom ""

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

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op []         = throwError $ NumArgs 1 []
unaryOp op [v]        = return $ op v
unaryOp op vals@(_:_) = throwError $ NumArgs 1 vals

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

--- Error handling ---
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError

showError :: LispError -> String
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (UnboundVar message varname)  = message ++ ": " ++ varname

type ThrowsError = Either LispError

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
