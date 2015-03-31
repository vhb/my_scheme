module Evaluator where
import Types
import Debug.Trace
import Errors
import Control.Monad.Error

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [] = throwError $ NumArgs 1 []
unaryOp op params =  return $ op $ head params

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

{-Take a symbol (atom) return the string associated-}
symbolToString :: LispVal -> LispVal
symbolToString (Atom x) = String x
symbolToString x = trace (show x) $ String "Expecting an atom"

{-Take a string and return the string associated-}
stringToSymbol :: LispVal -> LispVal
stringToSymbol (String x) = Atom x
stringToSymbol _ = String "Expecting a string"

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("not", unaryOp isNot),
              ("boolean?", unaryOp isBool),
              ("list?", unaryOp isList),
              ("symbol?", unaryOp isSymbol),
              ("char?", unaryOp isChar),
              ("string?", unaryOp isString),
              ("vector?", unaryOp isVector),
              ("symbol->string", unaryOp symbolToString),
              ("string->symbol", unaryOp stringToSymbol)
              ]

isNot :: LispVal -> LispVal
isNot (Bool x) = (Bool . not) x
isNot _ = Bool False

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _ = Bool False

isList :: LispVal -> LispVal
isList (List _) = Bool True
isList (DottedList _ _) = Bool True
isList _ = Bool False

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False

isChar :: LispVal -> LispVal
isChar (Char _) = Bool True
isChar _ = Bool False

isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _ = Bool False

isVector :: LispVal -> LispVal
isVector (Vector _) = Bool True
isVector _ = Bool False
