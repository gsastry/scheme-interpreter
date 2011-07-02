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

-- Make LispVal a member of the typeclass `Show`
instance Show LispVal where show = showVal

-- read a command line argument and attempt to parse the argument
-- as an expression.
main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)

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
		 x <- many (nonEscapeChars) <|> many (escapeChars)
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
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
		      Left err -> String $ "No match: " ++ show err
		      Right val -> val

-- `showVal`
-- Print out a string representation of possible LispVals
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
    "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"


-- `unwordsList`
-- Glue together the contents of a [LispVal] with spaces (like `unwords`)
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- `eval`
-- The beginnings of an evaluator. An evaluator maps a code data type to a 
-- a data data type. In our case (and in Lisps) the code is the data.
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

-- `apply`
-- Applies a supplied function to a list of arguments (LispVals)
-- and returns a LispVal
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

-- `primitives`
-- A mapping of primitives to their operations in Haskell
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
	      ("-", numericBinop (-)),
	      ("*", numericBinop (*)),
	      ("/", numericBinop div),
	      ("mod", numericBinop mod),
	      ("quotient", numericBinop quot),
	      ("remainder", numericBinop rem)]

-- `numericBinop`
-- The functions that we store in our primitives mapping are based on 
-- numericBinop.
-- numericBinop takes a Haskell primitive function and arguments, folds
-- that function over the list of arguments, and returns a Number'd answer.
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

-- `unpackNum`
-- Scheme is dynamically typed. Here, we're implementing weak typing, which
-- means that if a value CAN be interpreted as a number (for example, the
-- string "2") then we'll use it as such.
-- If we can't parse the number, return 0 (for now ... )
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in
			   if null parsed
			      then 0
			      else fst $ parsed !! 0
