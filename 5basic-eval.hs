-- H24.12.01 16:43 Eval Eval Eval!
module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import Data.Array
import Control.Monad.Error

main :: IO ()
main = do
  args <- getArgs
  result <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError result

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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> throwError $ Parser err
                   Right val -> return val

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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    otherwise -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
--eval v= String$show v

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $
                      NotFunction "Unrecognized primitive function args" func)
                    ($ args)
                    (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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
               ("string->symbol", unaryOp string2symbol),
               ("=", numBoolBinop (==)),
               ("/=", numBoolBinop (/=)),
               ("<", numBoolBinop (<)),
               (">", numBoolBinop (>)),
               ("<=", numBoolBinop (<=)),
               (">=", numBoolBinop (>=)),
               ("&&", boolBoolBinop (&&)),
               ("||", boolBoolBinop (||)),
               ("string=?", strBoolBinop (==)),
               ("string<?", strBoolBinop (<)),
               ("string>?", strBoolBinop (>)),
               ("string<=?", strBoolBinop (<=)),
               ("string>=?", strBoolBinop (>=))

             ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal]
                -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum nan = throwError $ TypeMismatch "number" nan

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v

symbolp, numberp, stringp, booleanp, pairp :: LispVal -> LispVal
symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
booleanp (Bool _)  = Bool True
booleanp   _       = Bool False
pairp (List _)     = Bool True
pairp (DottedList _ _) = Bool True
pairp _            = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""
--booleanp, symbolp, charp, vectorp, {-procedurep,-}
--  pairp, numberp, complexp, realp, rationalp, integerp, stringp{-, portp-} :: LispVal -> Bool
--booleanp

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool)
             -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notStr = throwError $ TypeMismatch "string" notStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool (Number 0) = return False
unpackBool (Number 1) = return True
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected: " ++ show expected
                                   ++ ", found " ++ show found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                        ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError
instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default
type ThrowsError = Either LispError

--trapError ::
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- ghc -package parsec -o 5basic-eval.exe --make 5basic-eval.hs
-- 5basic-eval "(< 2 4)"
-- 5basic-eval "(> 2 4)"
-- 5basic-eval "(>= 3 3)"
-- 5basic-eval "(string=? \"test\"  \"test\")"
-- 5basic-eval "(string<? \"abc\" \"bba\")"
