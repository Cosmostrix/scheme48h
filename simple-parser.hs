-- H24.11.24 17:30 toward Parsec
module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr $ args !! 0)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first : rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= return . Number . read

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"" <|> (char '\\' >> char '"'))
                 char '"'
                 return $ String x

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseNumber <|> parseString

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value"

-- ghc -package parsec -o simple_parser.exe --make simple-parser.hs
-- simple_parser "\"this is a string\""
-- simple_parser "\"asdhj\\"kl;\""
-- simple_parser "\"asdhj\\"kl;""