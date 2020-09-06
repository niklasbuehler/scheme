module Repl where
import Parse
import Eval
import System.IO
import System.Environment
import Control.Monad.Except
import Data.IORef

--- Evaluating ---
readExpr :: String -> ThrowsError LispVal
readExpr input = case parseScheme input of
  Left err  -> throwError $ Parser err
  Right val -> return val

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(Character _)                          = return val
eval env val@(String _)                             = return val
eval env val@(Real _)                               = return val
eval env val@(Rational _)                           = return val
eval env val@(Complex _)                            = return val
eval env val@(Number _)                             = return val
eval env val@(Vector _)                             = return val
eval env val@(Bool _)                               = return val
eval env (Atom id)                              = getVar env id
eval env val@(DottedList _ _)                       = return val
eval env (List [Atom "quote", val])                 = return val
eval env (List [Atom "if", pred, conseq, alt])      =
     do result <- eval env pred
        case result of
             Bool False -> eval env alt
             otherwise  -> eval env conseq
eval env (List [Atom "set!", Atom var, form])   =
     eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var
eval env (List (Atom func : args))              =
     mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

--- REPL ---
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do result <- prompt
                               if pred result
                                  then return ()
                                  else action result >> until_ pred prompt action

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

--- Environment & Variables ---
type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
                                alreadyDefined <- liftIO $ isBound envRef var
                                if alreadyDefined
                                    then setVar envRef var value >> return value
                                    else liftIO $ do
                                                     valueRef <- newIORef value
                                                     env <- readIORef envRef
                                                     writeIORef envRef ((var, valueRef) : env)
                                                     return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)
