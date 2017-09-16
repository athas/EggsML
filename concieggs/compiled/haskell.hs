module Main(main) where

import Test.QuickCheck.Gen
import Text.Printf
import Data.List
import Control.Monad
import System.Environment

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


function :: Int -> IO String
function n = do
  n <- generate $ choose (1,n)
  typeclass <- generate $ choose (True, False)
  types <- generate $ someTypes n
  let types' = concat $ intersperse (" " :: String) $ intersperse ("->" :: String) types
  return types'

twoTyped :: Int -> IO String
twoTyped n = do
  twotyped <- generate $ elements twoTypedData
  [a, b] <- generate $ someTypes 2
  return $ printf "%s %s %s" twotyped a b

funkyType :: Int -> IO String
funkyType 0 = generate someType
funkyType n = do
  nesting <- generate $ choose (0,n)
  twotpd <- twoTyped nesting
  lst <- list nesting
  generate $ frequency [(1, pure twotpd), (2, pure lst)]

list :: Int -> IO String
list 0 = do tp <- generate someType; return $ printf "[%s]" tp
list n = do lst <- list (n-1); return $ printf "[%s]" lst


funnyFunk :: String -> IO ()
funnyFunk filename = do
  [linje, start] <- generate $ vectorOf 2 $ choose (1, 100) :: IO [Int]
  slut <- generate $ choose (start, 100)
  expected <- function 2
  actual <- function 4
  fun_type <- funkyType 1
  more_fun_type <- funkyType 2

  putStrLn $ printf "%s:%d:%d-%d: error ..." filename linje start slut
  putStrLn $ printf "    * Couldn't match type '%s' with '%s'" expected actual
  putStrLn $ printf "      Expected type: %s" fun_type
  putStrLn $ printf "        Actual type: %s" more_fun_type
  putStrLn "..."

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> funnyFunk filename
    _ -> do lol_module <- generate $ elements goodModules
            let filename = printf "%s.hs" lol_module
            funnyFunk filename
