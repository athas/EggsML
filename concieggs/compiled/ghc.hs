module Main(main) where

import Test.QuickCheck.Gen
import Text.Printf
import Data.List
import Control.Monad
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
           ]

twoTypedData :: [String]
twoTypedData = [ "Data.Map"
               , "Either"
               ]
someType :: Gen String
someType = elements typeList1

someTypes :: Int -> Gen [String]
someTypes n = vectorOf n someType

dataTypes :: [String]
dataTypes = []


function :: Int -> Gen String
function n = do
  n <- choose (1,n)
  typeclass <- choose (True, False)
  types <- someTypes n
  let types' = concat $ intersperse (" " :: String) $ intersperse ("->" :: String) types
  return types'

twoTyped :: Int -> Gen String
twoTyped n = do
  twotyped <- elements twoTypedData
  [a, b] <- someTypes 2
  return $ printf "%s %s %s" twotyped a b

funkyType :: Int -> Gen String
funkyType 0 = someType
funkyType n = do
  nesting <- choose (0,n)
  twotpd <- twoTyped nesting
  lst <- list nesting
  frequency [(1, pure twotpd), (2, pure lst)]

funkyKind :: Gen String
funkyKind = do
  frequency [(1, return "*"), (2, ("* -> "++) <$> funkyKind)]

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
funnyFunk filename = do
  putStr =<< generate (errorMessage filename)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> funnyFunk filename
    _ -> do lol_module <- generate $ elements goodModules
            let filename = printf "%s.hs" lol_module
            funnyFunk filename
