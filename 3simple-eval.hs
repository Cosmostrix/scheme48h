-- H24.12.01 16:43 Eval Eval Eval!
module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import Data.Array

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

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
                   option ' ' (try (string "space" >> return ' ')
                                <|> try (string "newline" >> return '\n')
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
        <|> try parseNum
        <|> parseAtom
        <|> parseString
        <|> parseQuoted <|> parseQusaiquote
        <|> try parseUnquoteS <|> parseUnquote
        <|> parens parseList

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> String $ "No match: " ++ show err
                   Right val -> val

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = showFFloat Nothing contents ""
showVal (Ratio contents) = show (numerator contents) ++ "/"
                         ++ show (denominator contents)
showVal (Complex x) = showFFloat Nothing (realPart x) $ "+"
                    ++ showFFloat Nothing (imagPart x) "i"
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character '\n') = "#\\newline"
showVal (Character ' ') = "#\\space"
showVal (Character char) = "#\\" ++ [char]
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head
                               ++ " . " ++ showVal tail ++ ")"
showVal (Vector contents) = "#(" ++ unwordsList (elems contents) ++ ")"
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
--eval v= String$show v

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [ ("+", numericBinop (+)),
               ("-", numericBinop (-)),
               ("*", numericBinop (*)),
               ("/", numericBinop div),
               ("mod", numericBinop mod),
               ("quotient", numericBinop quot),
               ("remainder", numericBinop rem),
               ("boolean?", unaryOp booleanp),
               ("symbol?", unaryOp symbolp),
{-               ("char?", unaryOp charp),
               ("vector?", unaryOp vectorp),
--               ("procedure?", unaryOp procedurep),-}
               ("pair?", unaryOp pairp),
               ("number?", unaryOp numberp),{-
               ("complex?", unaryOp complexp),
               ("real?", unaryOp realp),
               ("rational?", unaryOp rationalp),
               ("integer?", unaryOp integerp),-}
               ("string?", unaryOp stringp),
--               ("port?", unaryOp portp),-}
               ("symbol->string", unaryOp symbol2string),
               ("string->symbol", unaryOp string2symbol)
             ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

symbolp, numberp, stringp, booleanp, pairp :: LispVal -> LispVal
symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
booleanp (Bool _)  = Bool True
booleanp   _       = Bool False
pairp   (List _)   = Bool True
pairp   (DottedList _ _) = Bool True
pairp   _          = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""
 
--booleanp, symbolp, charp, vectorp, {-procedurep,-}
--  pairp, numberp, complexp, realp, rationalp, integerp, stringp{-, portp-} :: LispVal -> Bool
--booleanp 




-- ghc -package parsec -o 3simple_eval.exe --make 3simple-eval.hs
-- 3simple_eval "(+ 2 2)"
-- 3simple_eval "(+ 2 (- 4 1))"
-- 3simple_eval "(- (+ 4 6 3) 3 5 2)"
