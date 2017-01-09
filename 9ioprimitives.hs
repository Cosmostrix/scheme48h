-- H24.12.06 12:53 Hello World
module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import Data.Array
import Control.Monad.Error
import System.IO
import Data.IORef

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOnce args

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $
    (liftThrows $ readExpr expr) >>= eval env

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

runOnce :: [String] -> IO ()
runOnce args = do
  env <- primitiveBindings
         >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
       >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>=
    until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

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
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

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

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
                             Left err -> throwError $ Parser err
                             Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)


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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
       Nothing -> ""
       Just arg -> " . " ++ arg) ++ ") ...)"
showVal (IOFunc _) = "#<IO Primitive>"
showVal (Port _) = "#<IO port>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList(Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
    func <- eval env function
    argVars <- mapM (eval env) args
    apply func argVars
eval env badForm =
    throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args)
                >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $
                  bindVars env [(argName, List $ remainingArgs)]
              Nothing -> return env
apply (IOFunc func) args = func args

makeFunc varargs env params body =
    return $ Func (map showVal params) varargs body env

makeNormalFunc = makeFunc Nothing

makeVarargs = makeFunc . Just . showVal

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
               ("string>=?", strBoolBinop (>=)),
               ("car", car),
               ("cdr", cdr),
               ("cons", cons),
               ("eq?", eqv),
               ("eqv?", eqv),
               ("equal?", equal)
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

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x : xs)
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool x), (Bool y)] = return $ Bool $ x == y
eqv [(Number x), (Number y)] = return $ Bool $ x == y
eqv [(String x), (String y)] = return $ Bool $ x == y
eqv [(Atom x), (Atom y)] = return $ Bool $ x == y
eqv [(DottedList xs x), (DottedList ys y)] =
    eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List x), (List y)] =
    return $ Bool $ (length x == length y) && (all eqvPair $ zip x y)
  where eqvPair (x1, x2) = case eqv [x1, x2] of
                             Left err -> False
                             Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals x y (AnyUnpacker unpacker) =
  do
    unpacked1 <- unpacker x
    unpacked2 <- unpacker y
    return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [x, y] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals x y)
    [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [x, y]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

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

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

primitiveBindings :: IO Env
primitiveBindings =
    nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                  ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = (var, constructor func)

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var =
    readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . (flip writeIORef value))
        (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
     then setVar envRef var value >> return value
     else liftIO $ do
       valueRef <- newIORef value
       env <- readIORef envRef
       writeIORef envRef ((var, valueRef) : env)
       return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename


-- ghc -package parsec -fglasgow-exts -o 9ioprim.exe --make 9ioprimitives.hs
-- 9ioprim.exe
-- Lisp>>> (load "stdlib.scm")
-- Lisp>>> (map (lambda (x) (+ 1 x)) '(1 2 3 5 6 7 8 9))
-- (2 3 4 6 7 8 9 10)
-- Lisp>>> (filter odd? '(1 2 3 5 6 7 8 9))
-- (1 3 5 7 9)
-- Lisp>>> quit
-- 