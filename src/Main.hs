module Main where
import System.Environment
import Parser
import Evaluator

main :: IO()
main = getArgs >>= print . eval . readExpr . head
{-main = do-}
  {-args <- getArgs-}
  {-print (eval (readExpr (head args)))-}

{-main:: IO()-}
{-main = getArgs <<= print . eval . readExpr . head-}
{-main :: IO()-}
{-main = do-}
  {-args <- getArgs-}
  {-putStrLn (readExpr (head args))-}

