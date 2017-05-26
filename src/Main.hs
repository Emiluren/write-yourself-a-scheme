module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except
import System.IO
import Data.IORef
import Data.Maybe (isJust)
import System.Console.Haskeline
import System.Environment (getArgs)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
    | Func { params :: [String]
           , vararg :: Maybe String
           , body :: [LispVal]
           , closure :: Env
           }
    | IOFunc ([LispVal] -> IOThrowsError LispVal)
    | Port Handle

data LispError
    = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String LispVal
    | UnboundVar String String

type ThrowsError = Either LispError

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = numOps
    [ ("+", (+))
    , ("-", (-))
    , ("*", (*))
    , ("/", div)
    , ("mod", mod)
    , ("quotient", quot)
    , ("remainder", rem)
    ] ++ numBoolOps
    [ ("=", (==))
    , ("<", (<))
    , (">", (>))
    , ("/=", (/=))
    , (">=", (>=))
    , ("<=", (<=))
    ] ++ boolBoolOps
    [ ("&&", (&&))
    , ("||", (||))
    ] ++ strBoolOps
    [ ("string=?", (==))
    , ("string<?", (<))
    , ("string>?", (>))
    , ("string<=?", (<=))
    , ("string>=?", (>=))
    ] ++
    [ ("car", car)
    , ("cdr", cdr)
    , ("cons", cons)
    , ("eq?", eqv)
    , ("eqv?", eqv)
    , ("equal?", eqv)
    ]
    where
        ffmap = fmap . fmap
        numOps = ffmap numericBinop
        numBoolOps = ffmap $ boolBinop unpackNum
        boolBoolOps = ffmap $ boolBinop unpackBool
        strBoolOps = ffmap $ boolBinop unpackStr

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
    [ ("apply", applyProc)
    , ("open-input-file", makePort ReadMode)
    , ("open-output-file", makePort WriteMode)
    , ("close-input-port", closePort)
    , ("close-output-port", closePort)
    , ("read", readProc)
    , ("write", writeProc)
    , ("read-contents", readContents)
    , ("read-all", readAll)
    ]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (ioFuncs ++ primitiveFuncs)
    where ioFuncs = map (makeFunc IOFunc) ioPrimitives
          primitiveFuncs = map (makeFunc PrimitiveFunc) primitives
          makeFunc constructor (var, func) = (var, constructor func)

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = Number . foldl1 op <$> mapM unpackNum params

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op [arg1, arg2] = do
    left <- unpacker arg1
    right <- unpacker arg2
    return $ Bool $ left `op` right
boolBinop _ _ args = throwError $ NumArgs 2 args

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
-- unpackNum (String s) =
--     let parsed = reads s :: [(Integer, String)] in
--         if null parsed
--         then throwError $ TypeMismatch "number" $ String s
--         else return $ fst $ head parsed
-- unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
-- unpackStr (Number n) = return $ show n
-- unpackStr (Bool b) = return $ show b
unpackStr notBool = throwError $ TypeMismatch "boolean" notBool

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

parseString :: Parser LispVal
parseString = do
    void $ char '"'
    x <- many (noneOf "\"")
    void $ char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = DottedList
    <$> endBy parseExpr spaces
    <*> (char '.' >> spaces >> parseExpr)

parseQuoted :: Parser LispVal
parseQuoted = do
    void $ char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do void $ char '('
           x <- try parseList <|> parseDottedList
           void $ char ')'
           return x

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input =
    case parse parser "lisp" input of
        Left err -> throwError $ Parser err
        Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number n) = show n
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList x xs) = "(" ++ unwordsList x ++ " . " ++ show xs ++ ")"
    show (PrimitiveFunc _) = "<primitive>"
    show Func { vararg = varargs, params = args } =
        "(lambda (" ++ unwords (show <$> args) ++
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg)
        ++ ") ...)"
    show (Port _) = "<IO port>"
    show (IOFunc _) = "<IO primitive>"

instance Show LispError where
    show (UnboundVar m v) = m ++ ": " ++ v
    show (BadSpecialForm m f) = m ++ ": " ++ show f
    show (NotFunction m f) = m ++ ": " ++ show f
    show (NumArgs e f) = "Expected " ++ show e
        ++ " args; found values " ++ unwordsList f
    show (TypeMismatch e f) = "Invalid type: expected " ++ e
        ++ ", found " ++ show f
    show (Parser p) = "Parse error at " ++ show p

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom var) = getVar env var
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", predicate, conseq, alt]) = do
    result <- eval env predicate >>= liftThrows <$> unpackBool
    if result
        then eval env conseq
        else eval env alt
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) =
    load filename >>= fmap last . mapM (eval env)
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
    if length params /= length args && varargs == Nothing
       then throwError $ NumArgs (toInteger $ length params) args
       else liftIO (bindVars closure $ zip params args)
            >>= bindVarArgs varargs
            >>= evalBody
            where
                remainingArgs = drop (length params) args
                evalBody env = last <$> mapM (eval env) body
                bindVarArgs arg env =
                    case arg of
                        Just argName -> liftIO $
                            bindVars env [(argName, List remainingArgs)]
                        Nothing -> return env
apply (IOFunc func) args = func args
apply notFun _ = throwError $ NotFunction "Not a function" notFun

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs y] = return $ DottedList (x:xs) y
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

cmp :: Eq a => a -> a -> ThrowsError LispVal
cmp v1 v2 = return $ Bool $ v1 == v2

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool b1, Bool b2] = cmp b1 b2
eqv [Number n1, Number n2] = cmp n1 n2
eqv [String s1, String s2] = cmp s1 s2
eqv [DottedList xs x, DottedList ys y] = eqv [List (x:xs), List (y:ys)]
eqv [List l1, List l2] =
    return $ Bool $ (length l1 == length l2) &&
        all eqvPair (zip l1 l2)
    where eqvPair (x1, x2) =
              case eqv [x1, x2] of
                  Right (Bool val) -> val -- This should be the only possibility
                  Left _ -> False
                  Right _ -> False
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
    result <- prompt
    unless (predicate result) $
        action result >> until_ predicate prompt action

runRepl :: IO ()
-- runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint
runRepl = primitiveBindings >>= runInputT defaultSettings . loop
    where loop :: Env -> InputT IO ()
          loop env  = do
              minput <- getInputLine "Lisp>>> "
              case minput of
                  Nothing -> return ()
                  Just "quit" -> return ()
                  Just ":q" -> return ()
                  Just "exit" -> return ()
                  Just input -> liftIO (evalAndPrint env input) >> loop env

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

printEnv :: [(String, IORef LispVal)] -> IO ()
printEnv env = do
    allVals <- mapM (mapM readIORef) env
    print allVals

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
        (liftIO . flip writeIORef value)
        (lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv binds env = (++ env) <$> mapM addBinding binds
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map show params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . show

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = String <$> liftIO (readFile filename)

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename

runFile :: [String] -> IO ()
runFile (a:as) = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String as)]
    runIOThrows (show <$> eval env (List [Atom "load", String a])) >>= hPutStrLn stderr

main :: IO ()
main = do
    args <- getArgs
    if null args
        then runRepl
        else runFile args
