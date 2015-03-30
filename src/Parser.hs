module Parser where
import Text.ParserCombinators.Parsec hiding (spaces)
import Debug.Trace
import Control.Monad
import qualified Data.Vector as Vec
import Numeric (readOct, readHex)
import Types


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  (return . Atom) (first:rest)

parseBool :: Parser LispVal
parseBool = do
    char '#'
    l <- oneOf ['t', 'f']
    return $ case l of
      't' -> Bool True
      'f' -> Bool False

retParseNumber = return . Number . fst . head

parseHexNumber :: Parser LispVal
parseHexNumber = do
  char '0'
  char 'x'
  n <- many (oneOf "0123456789ABCEDF")
  (retParseNumber . readHex) n

parseOctNumber :: Parser LispVal
parseOctNumber = do
  char '0'
  char 'o'
  n <- many (oneOf "01234567")
  (retParseNumber . readOct) n

readBin :: String -> Integer
readBin = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
  where c2i c = if c == '0' then 0 else 1

parseBinNumber :: Parser LispVal
parseBinNumber = do
  char '0'
  char 'b'
  n <- many (oneOf "01")
  (return . Number . readBin) n

parseDecimalNumber :: Parser LispVal
parseDecimalNumber = do
  n <- many1 digit
  return $ (Number . read) n

parseNumber :: Parser LispVal
parseNumber = parseDecimalNumber
  <|> parseHexNumber
  <|> parseOctNumber
  <|> parseBinNumber

parseFloat :: Parser LispVal
parseFloat = do
  n <- many1 digit
  char '.'
  n2 <- many1 digit
  return $ (Float . read) (n ++ "." ++ n2)

parseEscapedChars :: Parser Char
parseEscapedChars = do
  char '\\'
  c <- oneOf ['"', 'n', 't', 'r', 'b', '\\']
  return $ case c of
    't' -> '\t'
    'n' -> '\n'
    'r' -> '\r'
    'b' -> '\b'
    '"' -> '\"'
    '\\' -> '\\'

parseCharacter :: Parser LispVal
parseCharacter = do
  char '\''
  c <- (parseEscapedChars <|> noneOf ['\''])
  char '\''
  return $ Char c

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (parseEscapedChars <|> (noneOf ['\"']))
  char '"'
  return $ String x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseVector :: Parser LispVal
parseVector = do
  string "#("
  l <- sepBy parseExpr spaces
  char ')'
  return $ Vector (Vec.fromList l)


{-TODO: think about changing thoses atoms to proper node in the AST-}
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseBackquote :: Parser LispVal
parseBackquote = do
  string "`"
  l <- parseExpr
  return $ List [Atom "Quasiquote", l]

parseExpr :: Parser LispVal
parseExpr = parseString
  <|> try parseVector
  <|> try parseBool
  <|> try parseAtom
  <|> try parseFloat
  <|> try parseNumber
  <|> try parseQuoted
  <|> try parseCharacter
  <|> try parseBackquote
  <|> do
    char '('
    x <- try parseList 
      <|> try parseDottedList
    char ')'
    return x

{- TODO: add support for all numeric types: long, etc...  -}

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val
