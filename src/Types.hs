module Types where
import qualified Data.Vector as Vec
import Debug.Trace

data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Vector (Vec.Vector LispVal)
  | Number Integer
  | String String
  | Bool Bool
  | Char Char
  | Float Float deriving Show

{-Better for debug-}
{-instance Show LispVal where -}
  {-show = showVal-}

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
