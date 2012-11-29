-- H24.11.24 17:30 toward Parsec
module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import Data.Array

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
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Vector (Array Int LispVal)
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

parseSigned :: (Real r) => ReadS r -> Parser [Char] -> Parser r
parseSigned reader lexer = liftM (fst . head . readSigned reader) (do
  sign <- char '-' <|> return '0'
  num <- lexer
  return (sign : num))

lexFloat :: Parser [Char]
lexFloat = do x <- many1 digit
              char '.'
              y <- many digit
              return $ x ++ "." ++ y

parseFloat :: Parser LispVal
parseFloat = (liftM Float $ parseSigned readFloat lexFloat)

parseDigits :: Parser LispVal
parseDigits = (liftM Number $ parseSigned readDec (many1 digit))

parseRatio :: Parser LispVal
parseRatio = do
  x <- parseSigned readDec (many1 digit)
  char '/'
  y <- parseSigned readDec (many1 digit)
  return $ Ratio (x % y)

toDouble :: LispVal -> Double
toDouble (Number n) = fromIntegral n
toDouble (Float f) = f

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseFloat <|> parseDigits)
                  char '+'
                  y <- (try parseFloat <|> parseDigits)
                  char 'i'
                  return $ Complex (toDouble x :+ toDouble y)

parseNum :: Parser LispVal
parseNum = try parseComplex
           <|> try parseRatio
           <|> try parseFloat
           <|> parseDigits

parseNumber :: Parser LispVal
parseNumber = liftM Number
                (parseBin <|> parseOct <|> parseDig <|> parseHex)
  where 
        parseBin = char 'b' >> parseSigned readBin (many1 $ oneOf "01")
        parseOct = char 'o' >> parseSigned readOct (many1 octDigit)
        parseDig = char 'd' >> parseSigned readDec (many1 digit)
        parseHex = char 'x' >> parseSigned readHex (many1 hexDigit)
        readBin  = readInt 2 (\x->x=='0'||x=='1') (\x->if x=='0' then 0 else 1)

parseSharpSyntax :: Parser LispVal
parseSharpSyntax = char '#' >> (parseBool
                     <|> parseNumber
                     <|> parseCharacter
                     <|> parseVector)

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

parseList :: Parser LispVal
parseList = do
  body <- sepEndBy parseExpr spaces
  if length body == 0 then return $ List []
  else parseTail >>= \tail-> return $
    case tail of
      Just (List l)         -> List (body ++ l)
      Just (DottedList h t) -> DottedList (body ++ h) t
      Just val              -> DottedList body val
      Nothing               -> List body

parseTail :: Parser (Maybe LispVal)
parseTail = liftM Just (char '.' >> skipMany space >> parseExpr)
            <|> return Nothing

makeSyntaxSugarParser :: Parser a -> String -> Parser LispVal
makeSyntaxSugarParser ch symbol =
  ch >> parseExpr >>= (\x -> return $ List [Atom symbol, x])
  
parseQuoted :: Parser LispVal
parseQuoted = makeSyntaxSugarParser (char '\'') "quote"

parseQusaiquote :: Parser LispVal
parseQusaiquote = makeSyntaxSugarParser (char '`') "qusaiquote"

parseUnquote :: Parser LispVal
parseUnquote = makeSyntaxSugarParser (char ',') "unquote"

parseUnquoteS :: Parser LispVal
parseUnquoteS = makeSyntaxSugarParser (string ",@") "unquote-splicing"

parens :: Parser a -> Parser a
parens = between (char '(' >> skipMany space) (char ')')

parseVector :: Parser LispVal
parseVector = do
  arrayValues <- parens (sepBy parseExpr spaces)
  return $ Vector (listArray (0,(length arrayValues - 1)) arrayValues)

parseExpr :: Parser LispVal
parseExpr = parseSharpSyntax
        <|> parseNum
        <|> parseAtom
        <|> parseString
        <|> parseQuoted <|> parseQusaiquote
        <|> try parseUnquoteS <|> parseUnquote
        <|> parens parseList

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value: " ++ show val

-- ghc -package parsec -o simple_parser.exe --make simple-parser.hs
-- simple_parser () (a) "(a . a)" "(a . ())" "(a . (a . a))"
-- simple_parser "(a a)" "(a a . a)" "(a a . ())" (a.(a.(a.())))
