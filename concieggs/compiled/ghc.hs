module Main(main) where

import Test.QuickCheck.Gen
import Text.Printf
import Data.List
import System.Environment

data FunctionIsh a = Atom a | Arrow (FunctionIsh a) (FunctionIsh a)

goodModules :: [String]
goodModules = [ "funkyGen"
              , "Curves"
              , "Subs"
              , "TypeClasses"
              , "Arithmetic"
              , "LinAlg"
              , "euler3"
              ]
typeList1 :: [String]
typeList1 = [ "String"
            , "Integer"
            , "Person"
            , "Expr"
            , "()"
            , "Char"
            , "Double"
            , "a"
           ]

typeClasses :: [String]
typeClasses = [ "Arbitrary"
              , "Traversable"
              , "Eq"
              , "Ord"
              , "Show"
              ]

twoTypedData :: [String]
twoTypedData = [ "Data.Map"
               , "Either"
               ]

oneTypedData :: [String]
oneTypedData = [ "Maybe"
               , "Gen"
               , ""
               ]

simpleType :: Gen String
simpleType = elements typeList1

someType :: Gen String
someType = frequency [(3, simpleType), (1,twoTyped), (1,twoTyped)]

typeDefinition :: Int -> Gen String
typeDefinition n = do
    types <- vectorOf n someType
    return $ intercalate (" " :: String) $ intersperse ("->" :: String) types

function :: Int -> Gen String
function n = do
  n' <- choose (1,n)
  typeClassed <- choose (True, False)
  if typeClassed
    then do
      typeclass <- elements typeClasses
      let typeclassPrefix = typeclass ++ " a => "
      types <- suchThat (typeDefinition n') (isInfixOf " a ")
      return $ typeclassPrefix ++ types
    else typeDefinition n'

twoTyped :: Gen String
twoTyped = do
  twotyped <- elements twoTypedData
  [a, b] <- vectorOf 2 simpleType
  return $ printf "%s %s %s" twotyped a b

oneTyped :: Gen String
oneTyped = do
  onetyped <- elements oneTypedData
  a <- someType
  return $ printf "%s %s" onetyped a

funkyType :: Int -> Gen String
funkyType 0 = someType
funkyType n = do
  nesting <- choose (0,n)
  twotpd <- twoTyped
  onetpd <- oneTyped
  lst <- list nesting
  frequency [(1, pure twotpd), (2, pure lst), (1, pure onetpd)]

funkyKind :: Gen String
funkyKind = frequency [(1, return "*"), (2, ("* -> "++) <$> funkyKind)]

list :: Int -> Gen String
list 0 = do tp <- someType; return $ printf "[%s]" tp
list n = do lst <- list (n-1); return $ printf "[%s]" lst

simpleError :: String -> Gen String
simpleError filename = do
  [linje, start] <- vectorOf 2 $ choose (1, 100) :: Gen [Int]
  slut <- choose (start, 100)
  expected <- function 2
  actual <- function 4
  fun_type <- funkyType 1
  more_fun_type <- funkyType 2

  return $ unlines [ printf "%s:%d:%d-%d: error ..." filename linje start slut
                   , printf "    * Couldn't match type '%s' with '%s'" expected actual
                   , printf "      Expected type: %s" fun_type
                   , printf "        Actual type: %s" more_fun_type
                   , "..." ]

kindSignatureError :: String -> Gen String
kindSignatureError filename = do
  kind <- funkyKind
  tfam <- someType
  [linje, start] <- vectorOf 2 $ choose (1, 100) :: Gen [Int]
  return $ unlines [ printf "%s:%d:%d: error:" filename linje start
                   , printf "Illegal kind signature: ‘%s’" kind
                   , "Perhaps you intended to use KindSignatures"
                   , printf "In the declaration for type family ‘%s’" tfam
                   ]

errorMessage :: String -> Gen String
errorMessage filename =
  frequency [(5, simpleError filename),
             (1, kindSignatureError filename)]

funnyFunk :: String -> IO ()
funnyFunk filename = putStr =<< generate (errorMessage filename)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> funnyFunk filename
    _ -> do lol_module <- generate $ elements goodModules
            let filename = printf "%s.hs" lol_module
            funnyFunk filename
