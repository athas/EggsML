module Fast.FastInterpreter
       ( runProg
       , Error (..)
       )
       where

import Fast.FastAST

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe

-- ^ Any runtime error.  You may add more constructors to this type
-- (or remove the existing ones) if you want.  Just make sure it is
-- still an instance of 'Show' and 'Eq'.
data Error = Error String
             deriving Eq

instance Show Error where
  show (Error s) = "Interpreter error:\n" ++ s

-- | Give the printed representation of a value.
printed :: Value -> String
printed (IntValue x) = show x
printed (StringValue s) = s
printed (ReferenceValue ref) = "#<object " ++ show ref ++ ">"
printed (TermValue (Term sym vs)) =
  sym ++ "(" ++ intercalate ", " (map printed vs) ++ ")"

type Store k v = [(k, v)]

insertInStore :: Eq k => k -> v -> Store k v -> Store k v
insertInStore k v [] = [(k,v)]
insertInStore k v ((k2,v2) : kvs) =
  if k == k2 then (k,v) : kvs
  else (k2,v2) : insertInStore k v kvs

lookupInStore :: Eq k => k -> Store k v -> Maybe v
lookupInStore _ [] = Nothing
lookupInStore k ((k2,v):kvs) =
  if k == k2 then Just v
  else lookupInStore k kvs

type GlobalStore = Store ObjectReference ObjectState
type ObjectFields = Store Name Value
type MethodVariables = Store Name Value

data GlobalState = GlobalState { globalStateStore :: GlobalStore
                               , globalStateCounter :: ObjectReference
                               }

initialGlobalState :: GlobalState
initialGlobalState = GlobalState [] 0

data ObjectState = ObjectState { objectFields :: ObjectFields
                               , objectClass :: ClassDecl
                               }

data MethodState = MethodState { methodObject :: ObjectState
                               , methodVariables :: MethodVariables
                               }

data FastM a = FastM {
  runFastM :: Prog -> GlobalState
           -> Either Error (a, String, GlobalState)
  }

instance Functor FastM where
  fmap = liftM

instance Applicative FastM where
  pure = return
  (<*>) = ap

instance Monad FastM where
  return x = FastM $ \_ s -> Right (x,[],s)
  FastM f >>= m = FastM $ \prog s ->
    case f prog s of
      Left e       -> Left e
      Right (x,output1,s') ->
        case runFastM (m x) prog s' of
          Right (y,output2,s'') ->
            Right (y,output1 ++ output2,s'')
          Left e ->
            Left e
  fail s = FastM $ \_ _ -> Left $ Error s

askProg :: FastM Prog
askProg = FastM $ \prog s -> Right (prog, [], s)

getGlobalState :: FastM GlobalState
getGlobalState = FastM $ \_ s -> Right (s, [], s)

putGlobalState :: GlobalState -> FastM ()
putGlobalState s = FastM $ \_ _ -> Right ((), [], s)

modifyGlobalState :: (GlobalState -> GlobalState) -> FastM ()
modifyGlobalState f = do s <- getGlobalState
                         putGlobalState $ f s

modifyGlobalStore :: (GlobalStore -> GlobalStore) -> FastM ()
modifyGlobalStore f = do
  s <- getGlobalState
  putGlobalState $ s { globalStateStore = f $ globalStateStore s }

lookupObject :: ObjectReference -> FastM ObjectState
lookupObject k = do
  s <- getGlobalState
  case lookupInStore k $ globalStateStore s of
    Nothing -> fail $ "Unknown object: " ++ show k
    Just v  -> return v

setObject :: ObjectReference -> ObjectState -> FastM ()
setObject obj os =
  modifyGlobalState $ \s ->
    s { globalStateStore = insertInStore obj os $ globalStateStore s }

allocUniqID :: FastM ObjectReference
allocUniqID = do s <- getGlobalState
                 putGlobalState $
                   s { globalStateCounter = globalStateCounter s + 1 }
                 return $ globalStateCounter s

data FastMethodM a = FastMethodM {
  runFastMethodM :: ObjectReference -> MethodState
                 -> FastM (Either Value a, MethodState)
  }

instance Functor FastMethodM where
  fmap = liftM

instance Applicative FastMethodM where
  pure = return
  (<*>) = ap

instance Monad FastMethodM where
  return x = FastMethodM $ \_ s -> return (Right x, s)
  FastMethodM f >>= m = FastMethodM $ \me s -> do
    res <- f me s
    case res of (Left v, s') ->
                  return (Left v, s')
                (Right x, s') ->
                  runFastMethodM (m x) me s'
  fail = liftFastM . fail

liftFastM :: FastM a -> FastMethodM a
liftFastM m = FastMethodM $ \me (MethodState os vars) -> do
  modifyGlobalStore (insertInStore me os)
  x <- m
  os' <- lookupObject me
  return (Right x,
          MethodState os' vars)

askSelf :: FastMethodM ObjectReference
askSelf = FastMethodM $ \objectid s -> return (Right objectid, s)

bindVars :: [(Name, Value)] -> FastMethodM a -> FastMethodM a
bindVars kvs m = do
  vars <- getsMethodState methodVariables
  let shadowed = filter ((`elem` names) . fst) vars
      vars' = foldr (uncurry insertInStore) vars kvs
  modifyMethodState $ \s -> s { methodVariables = vars' }
  x <- m
  modifyMethodState $ \s ->
    s { methodVariables =
           foldr (uncurry insertInStore) (methodVariables s) shadowed
      }
  return x
  where names = map fst kvs

returnValue :: Value -> FastMethodM a
returnValue v = FastMethodM $ \_ s -> return (Left v, s)

getMethodState :: FastMethodM MethodState
getMethodState = FastMethodM $ \_ s -> return (Right s, s)

putMethodState :: MethodState -> FastMethodM ()
putMethodState s = FastMethodM $ \_ _ -> return (Right (), s)

getsMethodState :: (MethodState -> a) -> FastMethodM a
getsMethodState f = do s <- getMethodState
                       return $ f s

modifyMethodState :: (MethodState -> MethodState) -> FastMethodM ()
modifyMethodState f = do s <- getMethodState
                         putMethodState $ f s

getObjectState :: FastMethodM ObjectState
getObjectState = getsMethodState methodObject

putObjectState :: ObjectState -> FastMethodM ()
putObjectState os = modifyMethodState $ \s -> s { methodObject = os }

getsObjectState :: (ObjectState -> a) -> FastMethodM a
getsObjectState f = do s <- getObjectState
                       return $ f s

modifyObjectState :: (ObjectState -> ObjectState) -> FastMethodM ()
modifyObjectState f = do s <- getObjectState
                         putObjectState $ f s

findClassDecl :: Name -> FastM ClassDecl
findClassDecl name = do
  prog <- askProg
  case find ((== name) . className) prog of
    Nothing   -> fail $ "Unknown class: " ++ name
    Just decl -> return decl

createObject :: Name -> [Value] -> FastM ObjectReference
createObject name vs = do
  newID <- allocUniqID
  decl <- findClassDecl name
  val <- instantiate newID decl vs
  modifyGlobalStore (insertInStore newID val)
  return newID

instantiate :: ObjectReference -> ClassDecl -> [Value] -> FastM ObjectState
instantiate objectid decl vs =
  case classConstructor decl of
    Nothing -> return newObjectState
    Just (MethodDecl params body) -> do
      when (length vs /= length params) $
        fail $ "Wrong number of arguments to constructor of class " ++
               className decl ++ "."
      (_, s) <- evalMethodBody objectid (zip params vs) newObjectState body
      return s
  where newObjectState = ObjectState [] decl

sendMessageTo :: Value -> Value -> FastM Value
sendMessageTo (ReferenceValue ref) v = do
  obj <- lookupObject ref
  case mapMaybe (receiver v) $ classMethods $ objectClass obj of
    (bnds,exprs):_ -> do
      (retval,obj') <- evalMethodBody ref bnds obj exprs
      setObject ref obj'
      return retval
    [] ->
      case classReceive $ objectClass obj of
        Nothing -> fail "No valid recipient of message/method."
        Just receive -> do
          (retval, obj') <-
            evalMethodBody ref [(receiveParam receive, v)] obj $
            receiveBody receive
          setObject ref obj'
          return retval
sendMessageTo dest (TermValue (Term "print" [])) = do
  printValue dest
  return dest
sendMessageTo dest (TermValue (Term "printLn" [])) = do
  printValue dest
  printValue $ StringValue "\n"
  return dest
sendMessageTo (StringValue s) (TermValue (Term "toInt" [])) =
  case reads s of
    [(x, "")] -> return $ IntValue x
    _         -> fail $ "String '" ++ s ++ "' cannot be coerced to integer."
sendMessageTo dest v =
  fail $ "Cannot send message " ++ printed v ++ " to object " ++ printed dest ++ "."

printValue :: Value -> FastM ()
printValue val = FastM $ \_ s -> Right ((), printed val, s)

receiver :: Value -> NamedMethodDecl -> Maybe ([(Name,Value)], Exprs)
receiver (TermValue (Term name vs)) (NamedMethodDecl methodname decl)
  | name == methodname, length vs == length (methodParameters decl) =
    Just (zip (methodParameters decl) vs, methodBody decl)
receiver _ _ = Nothing

evalMethodBody :: ObjectReference
               -> [(Name, Value)]
               -> ObjectState
               -> Exprs
               -> FastM (Value, ObjectState)
evalMethodBody objectid args s body = do
  res <- runFastMethodM (evalExprs body) objectid s'
  case res of (Right val, s'') -> return (val, methodObject s'')
              (Left val,  s'') -> return (val, methodObject s'')
  where s' = MethodState s args

evalExprs :: [Expr] -> FastMethodM Value
evalExprs [] = return $ TermValue $ Term "nil" []
evalExprs [e] = evalExpr e
evalExprs (e:es) = evalExpr e >> evalExprs es

evalExpr :: Expr -> FastMethodM Value
evalExpr (IntConst v) = return $ IntValue v
evalExpr (StringConst s) = return $ StringValue s
evalExpr (TermLiteral s es) = do
  vs <- mapM evalExpr es
  return $ TermValue $ Term s vs
evalExpr Self = do
  me <- askSelf
  return $ ReferenceValue me
evalExpr (Plus e1 e2) = evalArithOp (+) e1 e2
evalExpr (Minus e1 e2) = evalArithOp (-) e1 e2
evalExpr (DividedBy e1 e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  case (v1, v2) of
    (IntValue x, IntValue y)
      | y == 0    -> fail "Division by zero"
      | otherwise -> return $ IntValue $ x `div` y
    _ ->
      fail "Wrong types of arguments to division"
evalExpr (Times e1 e2) = evalArithOp (*) e1 e2
evalExpr (Return e) = do
  v <- evalExpr e
  returnValue v
evalExpr (ReadVar name) = do
  vars <- getsMethodState methodVariables
  case lookupInStore name vars of
    Nothing -> fail $ "Unknown variable " ++ name
    Just v  -> return v
evalExpr (SetVar name e) = do
  v <- evalExpr e
  vars <- getsMethodState methodVariables
  modifyMethodState $ \s ->
    s { methodVariables = insertInStore name v vars }
  return v
evalExpr (SetField name e) = do
  v <- evalExpr e
  fields <- getsObjectState objectFields
  modifyObjectState $ \s ->
    s { objectFields = insertInStore name v fields }
  return v
evalExpr (ReadField name) = do
  fields <- getsObjectState objectFields
  case lookupInStore name fields of
    Nothing -> fail $ "Unknown field " ++ name
    Just v  -> return v
evalExpr (Match e cases) = do
  v <- evalExpr e
  case mapMaybe (matchCase v) cases of
    []             -> return $ TermValue $ Term "nil" []
    (bnds,exprs):_ -> bindVars bnds $ evalExprs exprs
evalExpr (SendMessage dest msg) = do
  msgv <- evalExpr msg
  destv <- evalExpr dest
  liftFastM $ sendMessageTo destv msgv
evalExpr (CallMethod dest name args) = do
  destv <- evalExpr dest
  argvals <- mapM evalExpr args
  liftFastM $ sendMessageTo destv $ TermValue $ Term name argvals
evalExpr (New classname args) = do
  vs <- mapM evalExpr args
  ref <- liftFastM $ createObject classname vs
  return $ ReferenceValue ref

matchCase :: Value -> (Pattern,Exprs) -> Maybe ([(Name, Value)], Exprs)
matchCase v (pat,exprs) =
  case matchPattern v pat of
    Just bnds -> Just (bnds,exprs)
    Nothing   -> Nothing

matchPattern :: Value -> Pattern -> Maybe [(Name, Value)]
matchPattern val (AnyValue name) =
  Just [(name, val)]
matchPattern (IntValue pat) (ConstInt v)
  | pat == v = Just []
matchPattern (IntValue _) _ = Nothing
matchPattern (StringValue pat) (ConstString s)
  | pat == s = Just []
matchPattern (StringValue _) _ = Nothing
matchPattern (TermValue (Term valsym subvals)) (TermPattern patsym patsubs)
  | valsym == patsym, length subvals == length patsubs =
    Just $ zip patsubs subvals
matchPattern _ _ = Nothing

evalArithOp :: (Integer -> Integer -> Integer) -> Expr -> Expr -> FastMethodM Value
evalArithOp f e1 e2 = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  case (v1, v2) of
    (IntValue x, IntValue y) ->
      return $ IntValue $ f x y
    _ ->
      fail "Wrong types of arguments to arithmetic binary operator"

runProg :: Prog -> [String] -> Either Error String
runProg prog args = case runFastM m prog initialGlobalState of
  Left e             -> Left e
  Right (_,output,_) -> Right output
  where m = createObject "Main" [TermValue (Term "args" $ map StringValue args)]
