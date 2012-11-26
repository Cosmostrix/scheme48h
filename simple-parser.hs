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
             | Character Char
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

parseSigned :: (Real r) => ReadS r -> Parser Char -> Parser r
parseSigned reader lexer = liftM (fst . head . readSigned reader) (do
  sign <- char '-' <|> return '0'
  num <- many1 lexer
  return (sign : num))

parseNum :: Parser LispVal
parseNum = liftM Number $ parseSigned readDec digit

parseNumber :: Parser LispVal
parseNumber = liftM Number
                (parseBin <|> parseOct <|> parseDig <|> parseHex)
  where 
        parseBin = char 'b' >> parseSigned readBin (char '0' <|> char '1')
        parseOct = char 'o' >> parseSigned readOct octDigit
        parseDig = char 'd' >> parseSigned readDec digit
        parseHex = char 'x' >> parseSigned readHex hexDigit
        readBin  = readInt 2 (\x->x=='0'||x=='1') (\x->if x=='0' then 0 else 1)

parseSharpSyntax :: Parser LispVal
parseSharpSyntax = char '#' >> (parseBool <|> parseNumber <|> parseCharacter)

escapedChars :: Parser Char
escapedChars = char '\\' >> oneOf "\\\"nrt" >>= \x ->
               return $ case x of
                 'n' -> '\n'
                 'r' -> '\r'
                 't' -> '\t'
                 _ -> x

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapedChars <|> noneOf "\"\\")
                 char '"'
                 return $ String x

parseCharacter :: Parser LispVal
parseCharacter = liftM Character (char '\\' >>
                   option ' ' ((string "space" >> return ' ')
                                <|> (string "newline" >> return '\n')
                                <|> do { x <- anyChar;
                                         notFollowedBy alphaNum;
                                         return x}))

parseExpr :: Parser LispVal
parseExpr = parseSharpSyntax <|> parseNum <|> parseAtom <|> parseString

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value: " ++ show val

-- ghc -package parsec -o simple_parser.exe --make simple-parser.hs
-- simple_parser #\
-- simple_parser #\space
-- simple_parser #\newline
-- simple_parser #\a
-- simple_parser #\A