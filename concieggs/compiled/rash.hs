{-# LANGUAGE TupleSections #-}
module Main (main) where

-- USER DOCUMENTATION

{-

`rash` is a state-saving pseudo-shell tailored for concieggs.

When its `read` instruction is encountered, the program exits.  When the program
is run again, it restarts at that `read` instruction and uses the command-line
arguments as the read input instead of using standard in.  This continues until
there are no more instructions to run.

`rash` is an assembly-like language.  Each line is an instruction.  It has the
following types of instructions:

  # Some text.
    A comment.
    `Instruction` constructor: None, optimized away

  a_var=some text
    Assignment of text 'some text' to variable `a_var`.
    `Instruction` constructor: `Assign`

  >command arg0 arg1
    Run `command arg0 arg1` in a shell, wait for it to finish, and print its
    output.
    `Instruction` constructor: `Run`

  read x
    Exit.  When restarted, read the command line arguments into the variable
    `x`.
    `Instruction` constructor: `Read`

  :your_label
    A label for jumping.
    `Instruction` constructor: None, optimized away

  j your_label
    Jump unconditionally to the `your_label` label.
    `Instruction` constructor: `Jump`

  jz somewhere
    Jump to the `somewhere` label if the previous command exited with return
    code 0.  If no command has previously run, the return code is 0.
    `Instruction` constructor: `JumpIfRetZero`

  exit
    Remove all state and exit.  This also happens if the end of the program is
    reached.
    `Instruction` constructor: `Exit`

  tribbles=>ls stuff
    Run `ls stuff` in a shell, wait for it to finish, and redirect its standard
    out into the `tribbles` variable.
    `Instruction` constructor: `AssignRun`

  <some text>grep -vi bar
    Run `grep -vi bar` with "some text" redirected into standard in, and print
    its output.  This can be used to simulate pipes.
    `Instruction` constructor: `Run`

  foo=<some text>grep -vi bar
    Run `grep -vi bar` with "some text" redirected into standard in, and
    redirect its standard out into the `foo` variable.
    `Instruction` constructor: `AssignRun`

To use a variable in text fields, write `${variable}`.  This can be used in
commands, assignment values, and command inputs, and it can be mixed with just
text.  For example, this:

  read url_base
  >grep http://${url_base}/robots.txt crawls

first reads command line arguments into the `url_base` variable, and then runs
`grep` with, among other text, the `url_base` value as the first argument.

Whitespace and lack thereof is significant, because why not?

For now, no special characters can be escaped.

-}


-- DEVELOPER DOCUMENTATION

{-

The pipeline of `rash` loading a new file looks like this:

  0. Parse the file into a list of intermediate `TempInstruction` instructions.

  1. Convert the `TempAssembly` into an optimized `Assembly` type, where jumps
     to labels are converted to jumps to absolute instructions, and variable
     names are converted to variable indexes.  Also turn all strings into
     `Data.Text.Text` values.

  2. Interpret.

-}


-- IMPORTS

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Control.Exception

import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Text.ShellEscape as TSE

import qualified System.Environment as Env
import qualified System.Directory as Dir
import qualified System.FilePath as Path
import qualified System.Exit as Exit
import qualified System.Process as Proc

import Data.Char
import qualified Data.Array.IArray as IA
import qualified Data.Array.MArray as MA
import qualified Data.Array.IO as IOA
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as May


-- SETTTINGS

maxNSteps :: Int
maxNSteps = 2000


-- MISCELLANEOUS

data RashPaths = RashPaths { pathOrig :: FilePath
                           , pathDir :: FilePath
                           , pathASM :: FilePath
                           , pathState :: FilePath
                           }
               deriving (Show)

xor :: Bool -> Bool -> Bool
xor x y = x /= y

-- | Run a process with optional standard in, and return the exit code and
-- standard out.
runProcess :: String -> Maybe String -> IO (Int, String)
runProcess cmdAndArgs stdinM = do
  let cp = Proc.shell cmdAndArgs
      stdin = case stdinM of
        Nothing -> ""
        Just s -> s
  (exitCode, stdout, _stderr) <- Proc.readCreateProcessWithExitCode cp stdin
  let i = case exitCode of
        Exit.ExitSuccess -> 0
        Exit.ExitFailure n -> n
  return (i, stdout)

type Sequence a = IA.Array Int a
type SequenceIO a = IOA.IOArray Int a

listToSequence :: [a] -> Sequence a
listToSequence xs = IA.listArray (0, length xs - 1) xs

sequenceToList :: Sequence a -> [a]
sequenceToList = IA.elems

sequenceLength :: Sequence a -> Int
sequenceLength = (+ 1) . snd . IA.bounds


-- END MODEL

type ID = Int

data Part = TextPart T.Text
          | IDPart Bool ID
          deriving (Read, Show)

data Instruction = Read { assignID :: ID
                        }
                   -- ^ Read a line into a register.

                 | Run { commandParts :: Sequence Part
                       , assignStdin :: Maybe (Sequence Part)
                       }
                   -- ^ Run a command with arguments.

                 | AssignRun { assignID :: ID
                             , commandParts :: Sequence Part
                             , assignStdin :: Maybe (Sequence Part)
                             }
                   -- ^ Run a command with arguments, and redirect its standard
                   -- out to a register.  Optionally, supply standard in.

                 | Assign { assignID :: ID
                          , contentParts :: Sequence Part
                          }
                   -- ^ Assign text to a register.

                 | JumpIfRetZero { jumpPos :: Int
                                 }
                   -- ^ Jump to an instruction position if the previous command
                   -- exited with return code 0.  If no command has previously
                   -- run, the return code is 0.

                 | Jump { jumpPos :: Int
                        }
                   -- ^ Jump to an instruction position.

                 | Exit
                   -- ^ Stop and exit.
              deriving (Read, Show)

data Assembly = Assembly (Sequence Instruction)
              deriving (Read, Show)

data Context = Context { contextAssembly :: Assembly
                       , contextReadArgs :: T.Text
                       , contextPaths :: RashPaths
                       }
             deriving (Show)

data State = State { statePC :: Int
                   , stateVars :: SequenceIO T.Text
                   , stateJustRestarted :: Bool
                   , statePrevExitCode :: Int
                   }

data IState = IState { iStatePC :: Int
                     , iStateVars :: Sequence T.Text
                     }
            deriving (Read, Show)


-- TEMPORARY MODEL

type TempID = String

type TempLabel = String

data TempPart = TempTextPart String
              | TempIDPart Bool TempID
              deriving (Show)

data TempInstruction = TempRead { tempAssignID :: TempID
                                }
                     | TempRun { tempCommandParts :: [TempPart]
                               , tempAssignStdin :: Maybe [TempPart]
                               }
                     | TempAssignRun { tempAssignID :: TempID
                                     , tempCommandParts :: [TempPart]
                                     , tempAssignStdin :: Maybe [TempPart]
                                     }
                     | TempAssign { tempAssignID :: TempID
                                  , tempContentParts :: [TempPart]
                                  }
                     | TempJumpIfRetZero { tempJumpPos :: TempLabel
                                         }
                     | TempJump { tempJumpPos :: TempLabel
                                }
                     | TempExit
                     | TempLabel { tempLabel :: TempLabel
                                 }
                 deriving (Show)

data TempAssembly = TempAssembly [TempInstruction]
                  deriving (Show)

formatTempInstruction :: TempInstruction -> String
formatTempInstruction inst = case inst of
  TempRead var -> "read " ++ var
  TempRun cmd Nothing -> ">" ++ formatStringParts cmd
  TempRun cmd (Just inp) -> "<" ++ (formatStringParts inp) ++ ">"
                            ++ formatStringParts cmd
  TempAssignRun var cmd Nothing -> var ++ "=" ++ ">" ++ formatStringParts cmd
  TempAssignRun var cmd (Just inp) -> var ++ "=" ++
                                      "<" ++ (formatStringParts inp) ++ ">"
                                     ++ formatStringParts cmd
  TempAssign var parts -> var ++ "=" ++ formatStringParts parts
  TempJumpIfRetZero label -> "jz " ++ label
  TempJump label -> "j " ++ label
  TempExit -> "exit"
  TempLabel label -> ":" ++ label

formatStringParts :: [TempPart] -> String
formatStringParts = concatMap formatStringPart

formatStringPart :: TempPart -> String
formatStringPart p = case p of
  TempTextPart s -> s
  TempIDPart b v -> (if b then "$'" else "$") ++ "{" ++ v ++ "}"


-- INTERPRETER

data InterpM a = InterpM { runInterpM :: Context -> State -> IO (a, State)
                         }

instance Monad InterpM where
  return x = InterpM $ \_ s -> return (x, s)

  m >>= f = InterpM $ \c s -> do
    (x', s') <- runInterpM m c s
    runInterpM (f x') c s'

instance Functor InterpM where
    fmap = liftM

instance Applicative InterpM where
    pure  = return
    (<*>) = ap

instance MonadIO InterpM where
  liftIO m = InterpM $ \_ s -> do
    r <- m
    return (r, s)

getContext :: InterpM Context
getContext = InterpM $ \c s -> return (c, s)

getState :: InterpM State
getState = InterpM $ \_ s -> return (s, s)

putState :: State -> InterpM ()
putState s = InterpM $ \_ _ -> return ((), s)

setPC :: Int -> InterpM ()
setPC pc = do
  s <- getState
  putState s { statePC = pc }

modifyPC :: (Int -> Int) -> InterpM ()
modifyPC f = do
  pc <- statePC <$> getState
  setPC $ f pc

setExitCode :: Int -> InterpM ()
setExitCode ec = do
  s <- getState
  putState s { statePrevExitCode = ec }

getVar :: Int -> InterpM T.Text
getVar i = do
  vars <- stateVars <$> getState
  liftIO $ MA.readArray vars i

setVar :: Int -> T.Text -> InterpM ()
setVar i t = do
  vars <- stateVars <$> getState
  liftIO $ MA.writeArray vars i t

interpret :: Context -> State -> IO ()
interpret c s = fst <$> runInterpM (interpretM 0) c s

interpretM :: Int -> InterpM ()
interpretM nSteps
  | nSteps > maxNSteps = liftIO $ putStrLn "Too many steps; stopping."
  | otherwise = do
    pc <- statePC <$> getState
    Assembly insts <- contextAssembly <$> getContext
    if pc >= sequenceLength insts
      then interpretInstruction Exit
      else do
      let instCur = insts IA.! pc
      interpretInstruction instCur
      interpretM (nSteps + 1)

emptyState :: Int -> IO State
emptyState nVars = do
  vars <- MA.newArray (0, nVars - 1) (T.pack "")
  return State { statePC = 0
               , stateVars = vars
               , stateJustRestarted = False
               , statePrevExitCode = 0
               }

freezeState :: State -> IO IState
freezeState st = do
  vars <- MA.freeze $ stateVars st
  return $ IState { iStatePC = statePC st
                  , iStateVars = vars
                  }

thawState :: IState -> IO State
thawState ist = do
  vars <- MA.thaw $ iStateVars ist
  return $ State { statePC = iStatePC ist
                 , stateVars = vars
                 , stateJustRestarted = True
                 , statePrevExitCode = 0
                 }

evalParts :: Sequence Part -> InterpM T.Text
evalParts ps = do
  let ps1 = extractParts ps
      ps2 = sequenceToList ps1
  ps3 <- mapM id ps2
  return $ T.concat ps3

extractParts :: Sequence Part -> Sequence (InterpM T.Text)
extractParts = IA.amap extractPart

extractPart :: Part -> InterpM T.Text
extractPart p = case p of
  TextPart t -> return t
  IDPart b v -> do r <- getVar v
                   return $ if b then shEsc r else r
          where shEsc = TE.decodeUtf8 . TSE.bytes . TSE.sh . TE.encodeUtf8

interpretInstruction :: Instruction -> InterpM ()
interpretInstruction inst = case inst of
  Read var -> do
    s <- getState
    c <- getContext

    if stateJustRestarted s
      then do
      setVar var $ contextReadArgs c
      putState s { stateJustRestarted = False }
      modifyPC (+ 1)
      setExitCode 0

      else do
      let paths = contextPaths c
          asm = contextAssembly c
      iState <- liftIO $ freezeState s
      liftIO $ writeFile (pathASM paths) (show asm)
      liftIO $ writeFile (pathState paths) (show iState)
      liftIO Exit.exitSuccess

  Run cmd stdinM -> do
    cmd' <- evalParts cmd
    stdinM' <- case stdinM of
      Nothing -> return Nothing
      Just stdin -> Just <$> evalParts stdin
    (ec, out) <- liftIO $ runProcess (T.unpack cmd') (T.unpack <$> stdinM')
    liftIO $ putStr out
    modifyPC (+ 1)
    setExitCode ec

  AssignRun v cmd stdinM -> do
    cmd' <- evalParts cmd
    stdinM' <- case stdinM of
      Nothing -> return Nothing
      Just stdin -> Just <$> evalParts stdin
    (ec, out) <- liftIO $ runProcess (T.unpack cmd') (T.unpack <$> stdinM')
    setVar v $ T.pack $ L.dropWhileEnd isSpace out
    modifyPC (+ 1)
    setExitCode ec

  Assign v parts -> do
    parts' <- evalParts parts
    setVar v parts'
    modifyPC (+ 1)
    setExitCode 0

  JumpIfRetZero p -> do
    ec <- statePrevExitCode <$> getState
    if (ec == 0)
      then setPC p
      else modifyPC (+ 1)
    setExitCode 0

  Jump p -> do
    setPC p
    setExitCode 0

  Exit -> do
    paths <- contextPaths <$> getContext
    liftIO $ flip catch (\e -> (e :: IOException) `seq` return ()) $ do
      Dir.removeFile $ pathASM paths
      Dir.removeFile $ pathState paths
    liftIO Exit.exitSuccess


-- PARSER

parseString :: String -> Either P.ParseError [TempInstruction]
parseString = P.parse instructionsP "input"

parseFile :: FilePath -> IO (Either P.ParseError [TempInstruction])
parseFile = P.parseFromFile instructionsP

symbol :: String -> P.Parser ()
symbol = P.try . void . P.string

isLineEnd :: Char -> Bool
isLineEnd c = c == '\n' || c == '\r'

lineEnd :: P.Parser ()
lineEnd = void $ P.satisfy isLineEnd

notLineEnd :: P.Parser Char
notLineEnd = P.satisfy (not . isLineEnd)

stringParts :: String -> [TempPart]
stringParts s = case P.parse stringPartsP "input" s of
  Left _ -> [] -- no errors anyway
  Right ps -> ps

stringPartsP :: P.Parser [TempPart]
stringPartsP = P.many stringPartP

stringPartP :: P.Parser TempPart
stringPartP = (TempIDPart False <$> idPartP "$")
              P.<|> (TempIDPart True <$> idPartP "$'")
              P.<|> (TempTextPart <$> textPartP)

idPartP :: String -> P.Parser TempID
idPartP p = do
  symbol (p ++ "{")
  var <- P.many1 (P.satisfy (/= '}'))
  symbol "}"
  return var

textPartP :: P.Parser String
textPartP = P.many1 (P.satisfy (/= '$'))

instructionsP :: P.Parser [TempInstruction]
instructionsP =
  May.catMaybes <$>
  (P.sepEndBy ((commentP >> return Nothing)
               P.<|> ((Just . TempLabel) <$> labelP)
               P.<|> (exitP >> return (Just TempExit))
               P.<|> ((Just . TempRead) <$> readP)
               P.<|> ((Just . TempJump) <$> jumpP)
               P.<|> ((Just . TempJumpIfRetZero) <$> jumpZeroP)
               P.<|> ((Just . uncurry TempRun) <$> runP)
               P.<|> (Just <$> assignGeneralP)
               P.<?> "instruction")
   (many lineEnd))

commentP :: P.Parser ()
commentP = do
  symbol "#"
  void $ P.many notLineEnd

labelP :: P.Parser TempLabel
labelP = do
  symbol ":"
  P.many1 notLineEnd

exitP :: P.Parser ()
exitP = P.try $ do
  symbol "exit"
  lineEnd

readP :: P.Parser TempID
readP = do
  symbol "read "
  P.many1 notLineEnd

jumpP :: P.Parser TempLabel
jumpP = do
  symbol "j "
  P.many1 notLineEnd

jumpZeroP :: P.Parser TempLabel
jumpZeroP = do
  symbol "jz "
  P.many1 notLineEnd

runP :: P.Parser ([TempPart], Maybe [TempPart])
runP = ((, Nothing) <$> runNoStdinP)
       <|> ((\(t, u) -> (t, Just u)) <$> runStdinP)

runNoStdinP :: P.Parser [TempPart]
runNoStdinP = do
  symbol ">"
  stringParts <$> P.many1 notLineEnd

runStdinP :: P.Parser ([TempPart], [TempPart])
runStdinP = do
  symbol "<"
  inp <- stringParts <$> P.many1 (P.satisfy (/= '>'))
  cmd <- runNoStdinP
  return (cmd, inp)

assignGeneralP :: P.Parser TempInstruction
assignGeneralP = do
  var <- P.many1 (P.satisfy (/= '='))
  symbol "="
  ((uncurry (TempAssignRun var) <$> runP)
   <|> (TempAssign var <$> (stringParts <$> P.many1 notLineEnd)))


-- RUNNER

runFile :: FilePath -> String -> IO ()
runFile fname readArgs = do
  paths <- rashPaths fname
  Dir.createDirectoryIfMissing True $ pathDir paths

  -- The following code is fragile.
  asmExists <- Dir.doesFileExist $ pathASM paths
  stateExists <- Dir.doesFileExist $ pathState paths
  when (asmExists `xor` stateExists) $ do
    when asmExists $ Dir.removeFile $ pathASM paths
    when stateExists $ Dir.removeFile $ pathState paths
    Exit.exitFailure
  let exists = asmExists -- any of the two will do

  (asm, state) <-
    if exists
    then do
      a <- (read <$> (readFile $ pathASM paths))
      i <- (read <$> (readFile $ pathState paths))
      s <- thawState i
      return (a, s)
    else do
      res <- parseFile fname
      case res of
        Left error -> do
          print error
          Exit.exitFailure
        Right insts -> do
          let (a, nVars) = asmTempToAsm $ TempAssembly insts
          s <- emptyState nVars
          return (a, s)

  let context = Context { contextAssembly = asm
                        , contextReadArgs = T.pack readArgs
                        , contextPaths = paths
                        }
  interpret context state

isTempLabel :: TempInstruction -> Bool
isTempLabel (TempLabel _) = True
isTempLabel _ = False

asmTempToAsm :: TempAssembly -> (Assembly, Int)
asmTempToAsm (TempAssembly insts) = (Assembly $ listToSequence insts'', nVars)
  where insts' :: [TempInstruction]
        insts' = filter (not . isTempLabel) insts

        insts'' :: [Instruction]
        insts'' = map instConv insts'

        instConv :: TempInstruction -> Instruction
        instConv ti = case ti of
          TempRead tid -> Read (varMap M.! tid)
          TempRun cmd stdinM -> Run (partsConv cmd) (partsConv <$> stdinM)
          TempAssignRun v cmd stdinM ->
            AssignRun (varMap M.! v) (partsConv cmd) (partsConv <$> stdinM)
          TempAssign v parts -> Assign (varMap M.! v) (partsConv parts)
          TempJumpIfRetZero label -> JumpIfRetZero (labelPoss M.! label)
          TempJump label -> Jump (labelPoss M.! label)
          TempExit -> Exit
          TempLabel _ -> error "FATAL: all labels should have been removed"

        partsConv :: [TempPart] -> Sequence Part
        partsConv = listToSequence . map partConv

        partConv :: TempPart -> Part
        partConv p = case p of
          TempTextPart s -> TextPart $ T.pack s
          TempIDPart b v -> IDPart b (varMap M.! v)

        labelPoss :: M.Map TempLabel Int
        labelPoss = M.fromList $ ps insts 0

        ps :: [TempInstruction] -> Int -> [(TempLabel, Int)]
        ps [] _ = []
        ps (i : is) n = case i of
          TempLabel label -> (label, n) : ps is n
          _ -> ps is (n + 1)

        nVars :: Int
        nVars = M.size varMap

        varMap :: M.Map TempID ID
        varMap = M.fromList $ zip (L.nub (L.concatMap instVars insts')) [0..]

        instVars :: TempInstruction -> [TempID]
        instVars inst = case inst of
          TempRead v -> [v]
          TempRun ps Nothing -> partsVars ps
          TempRun ps0 (Just ps1) -> partsVars (ps0 ++ ps1)
          TempAssignRun v ps Nothing -> [v] ++ partsVars ps
          TempAssignRun v ps0 (Just ps1) -> [v] ++ partsVars (ps0 ++ ps1)
          TempAssign v ps -> [v] ++ partsVars ps
          _ -> []

        partsVars :: [TempPart] -> [TempID]
        partsVars = May.catMaybes . map partVar

        partVar :: TempPart -> Maybe TempID
        partVar p = case p of
          TempTextPart _ -> Nothing
          TempIDPart _ v -> Just v

rashPaths :: FilePath -> IO RashPaths
rashPaths fname = do
  fnameCanon <- Dir.canonicalizePath fname
  dbDir <- Env.getEnv "CONCIEGGS_DB_DIR"
  let fnameSave = Path.combine dbDir "rash" ++ fnameCanon
      dirSave = fst $ Path.splitFileName fnameSave
      asmSave = fnameSave ++ ".asm"
      stateSave = fnameSave ++ ".state"
  return RashPaths { pathOrig = fname
                   , pathDir = dirSave
                   , pathASM = asmSave
                   , pathState = stateSave
                   }


-- MAIN

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    (fname : readArgs) -> runFile fname (L.intercalate " " readArgs)
    [] -> Exit.exitFailure
