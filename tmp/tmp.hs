module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO()
main = do putStrLn ("What is yer name?")
	  name <- getLine
	  putStrLn name
