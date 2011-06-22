-------------------------------------------------------------------------------
    -- Scheme Interpreter
    -- based on Write Yourself a Scheme in 48 Hours by Jonathan Tang
    -- a learning exercise!
-------------------------------------------------------------------------------
module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad

-- here we identify the data in the program
-- a data type can hold any Lisp value
data LispVal = Atom String
	     | List [LispVal]
	     | DottedList [LispVal] LispVal
	     | Number Integer
	     | String String
	     | Bool Bool


-- read a command line argument and attempt to parse the argument
-- as an expression.
main :: IO ()
main = do args <- getArgs
	  putStrLn (readExpr $ args !! 0)

-- a legal symbol is one of these: "!$%&|*+-/;<=>?@^_~"
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/;<=>?@^_~"

-- a parser for whitespace: skip 1 or more spaces
spaces :: Parser ()
spaces = skipMany1 space

-- a parser for strings
-- In our Scheme, a String is a `"` followed by some characters and a closing
-- `"`.
parseString :: Parser LispVal
parseString = do char '"'
		 x <- many (nonEscapeChars <|> escapeChars)
		 char '"'
		 return $ String x

-- a parser for non-escaped characters
nonEscapeChars :: Parser Char
nonEscapeChars = noneOf "\n\r\t\\\""

-- a parser for escape characters
-- An escape character is one of: \n, \r, \t, \\, or \"
escapeChars :: Parser Char
escapeChars = oneOf "\n\r\t\\\""

-- a parser for atoms
-- An atom is a letter or a symbol, followed by any number of letters, digits,
-- or symbols:
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
	       rest <- many (letter <|> digit <|> symbol)
	       let atom = first : rest
	       return $ case atom of
			     "#t" -> Bool True
			     "#f" -> Bool False
			     otherwise -> Atom atom

-- a parser for numbers
-- A Number is 1 or more digits in succession
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- | Alternate version of `parseNumber` using do notation
parseNumber' :: Parser LispVal
parseNumber' = do num <- many1 digit
		  return ( (Number . read) num)

-- | Parse a series of expressions separated by spaces as a List
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- | Parse the dotted list syntax: `(a b c d . e)`
parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
		     tail <- char '.' >> spaces >> parseExpr
		     return $ DottedList head tail

-- | Parse the quoted list: `'(l i s t)`
parseQuoted :: Parser LispVal
parseQuoted =  do char '\''
		  x <- parseExpr
		  return $ List [Atom "quote", x]
		  
-- A parser that accepts either a string, number, or atom
parseExpr :: Parser LispVal
parseExpr = parseAtom
	 <|> parseNumber
	 <|> parseString
	 <|> parseQuoted
	 <|> do char '('
		x <- (try parseList) <|> parseDottedList
		char ')'
		return x

-- `readExpr`
-- take a string and parse it
-- returns "No match" and an error, or "Found value" if successfully parsed
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
		      Left err -> "No match: " ++ show err
		      Right val -> "Found value"
