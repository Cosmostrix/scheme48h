-- H24.11.24 17:30 toward Parsec
module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric

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
             deriving Show

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
symbol1 :: Parser Char
symbol1 = oneOf "!$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol1
               rest <- many (letter <|> digit <|> symbol)
               return . Atom $ first : rest
               
parseBool :: Parser LispVal
parseBool = liftM Bool ((char 't' >> return True)
                         <|> (char 'f' >> return False))
parseNum :: Parser LispVal
parseNum = liftM (Number . fst . head . readDec) $ many1 digit

parseNumber :: Parser LispVal
parseNumber = liftM (Number . fst . head)
                (parseBin <|> parseOct <|> parseDig <|> parseHex)
  where 
        parseBin = liftM readBin $ char 'b' >> many1 (char '0' <|> char '1')
        parseOct = liftM readOct $ char 'o' >> many1 octDigit
        parseDig = liftM readDec $ char 'd' >> many1 digit
        parseHex = liftM readHex $ char 'x' >> many1 hexDigit
        readBin  = readInt 2 (\x->x=='0'||x=='1') (\x->if x=='0' then 0 else 1)

parseSharpSyntax :: Parser LispVal
parseSharpSyntax = char '#' >> (parseBool <|> parseNumber)

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"" <|> (char '\\' >> oneOf "\"nrt\\"))
                 char '"'
                 return $ String x

parseExpr :: Parser LispVal
parseExpr = parseSharpSyntax <|> parseNum <|> parseAtom <|> parseString

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value: " ++ show val

-- ghc -package parsec -o simple_parser.exe --make simple-parser.hs
-- simple_parser 01234567890
-- simple_parser #b1010
-- simple_parser #o1234567
-- simple_parser #d999999
-- simple_parser #x1abcdef