-- H24.11.24 17:30 toward Parsec
module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr $ args !! 0)

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value"

-- ghc -package parsec -o simple_parser.exe --make simple-parser.hs
-- simple_parser "   $"
-- simple_parser "   a"