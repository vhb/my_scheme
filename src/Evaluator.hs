module Evaluator where
import Types
import Debug.Trace

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval val@(List _) = val
eval _ = String "Other"

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp op params = op $ head params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
{-unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in -}
  {-if null parsed then 0 else fst $ head parsed-}
{-unpackNum (List [n]) = unpackNum n-}
unpackNum _ = 0

{-Take a symbol (atom) return the string associated-}
symbolToString :: LispVal -> LispVal
symbolToString (Atom x) = String x
symbolToString x = trace (show x) $ String "Expecting an atom"

{-Take a string and return the string associated-}
stringToSymbol :: LispVal -> LispVal
stringToSymbol (String x) = Atom x
stringToSymbol _ = String "Expecting a string"

primitives :: [(String, [LispVal] -> LispVal)]
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


apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives
