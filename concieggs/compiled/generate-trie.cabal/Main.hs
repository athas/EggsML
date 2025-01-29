{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Data.List (sort, sortBy, groupBy, intercalate)
import Data.Function (on, (&))
import Data.Maybe (mapMaybe)
import Data.Char (isAscii)

(&.) :: (a -> b) -> (b -> c) -> a -> c
(&.) = flip (.)

data Trie = NoMatch
          | Match String
          | Check Char Trie Trie
  deriving (Show)

separateFirstChar :: String -> Maybe (Char, String)
separateFirstChar = \case
  c : cs -> Just (c, cs)
  [] -> Nothing

constructTrie :: [(String, String)] -> Trie
constructTrie = \case
  [] ->
    NoMatch
  [("", target)] ->
    Match target
  pairs ->
    sort pairs
    & filter (all isAscii . fst) -- AT GØRE: Understøt æ, ø og å ved at arbejde på byteniveau.
    & mapMaybe (\(source, target) -> do
                   source' <- separateFirstChar source
                   pure (source', target))
    & groupBy (\((c, _), _) ((d, _), _) -> c == d)
    & map (\case
              group@(((c, _), _) : _) ->
                (c, map (\((_, source'), target) -> (source', target)) group)
              [] ->
                error "unexpected empty group")
    & sortBy (compare `on` (length . snd))
    & reverse
    & foldr (\(c, pairs') failureTrie ->
               Check c (constructTrie pairs') failureTrie) NoMatch

formatTrie :: Trie -> [String]
formatTrie = \case
  NoMatch ->
    [ "return EXIT_FAILURE;"
    ]
  Match target ->
    [ "puts(\"" ++ target ++ "\");"
    , "return EXIT_SUCCESS;"
    ]
  Check c successTrie failureTrie ->
    [ [ "if (*input == '" ++ [c] ++ "') {"
      , "input++;"
      ]
    , formatTrie successTrie
    , [ "} else {"
      ]
    , formatTrie failureTrie
    , [ "}"
      ]
    ]
    & concat

formatFullTrie :: Trie -> String
formatFullTrie trie =
  [ [ "#include <stdlib.h>"
    , "#include <stdio.h>"
    , "int main(int argc, char* argv[]) {"
    , "if (argc != 2) {"
    , "return EXIT_FAILURE;"
    , "}"
    , "char* input = argv[1];"
    ]
  , formatTrie trie
  , [ "}"
    ]
  ]
  & concat
  & intercalate "\n"

extractSourceAndTarget :: String -> (String, String)
extractSourceAndTarget =
  break (== ' ')
  &. (\case
         (source, ' ' : target) -> (source, target)
         (a, b) -> error ("unexpected line with components " ++ show (a, b)))

run :: String -> String
run = lines
      &. map extractSourceAndTarget
      &. constructTrie
      &. formatFullTrie

main :: IO ()
main = interact run
