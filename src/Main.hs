module Main where
import System.Environment
import Control.Monad
import Parser
import Evaluator
import Errors


main :: IO ()
main = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr (head args) >>= eval
     putStrLn $ extractValue $ trapError evaled

{-main = do-}
  {-args <- getArgs-}
  {-print (eval (readExpr (head args)))-}

{-main:: IO()-}
{-main = getArgs <<= print . eval . readExpr . head-}
{-main :: IO()-}
{-main = do-}
  {-args <- getArgs-}
  {-putStrLn (readExpr (head args))-}

