{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Data.List (sort, sortBy, groupBy, intercalate)
import Data.Function (on, (&))
import Data.Maybe (mapMaybe)
import GHC.Utils.Encoding.UTF8 (utf8EncodeByteString)
import Data.ByteString (unpack)
import Data.Word (Word8)

(&.) :: (a -> b) -> (b -> c) -> a -> c
(&.) = flip (.)

utf8Encode :: String -> [Word8]
utf8Encode = utf8EncodeByteString &. unpack

data Trie = NoMatch
          | Match [Word8]
          | Check Word8 Trie Trie
  deriving (Show)

separateFirstChar :: [Word8] -> Maybe (Word8, [Word8])
separateFirstChar = \case
  c : cs -> Just (c, cs)
  [] -> Nothing

constructTrie :: [([Word8], [Word8])] -> Trie
constructTrie = \case
  [] ->
    NoMatch
  [([], target)] ->
    Match target
  pairs ->
    sort pairs
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
    [ map (\c -> "putchar(" ++ show c ++ ");") target
    , [ "putchar('\\n');"
      , "return EXIT_SUCCESS;"
      ]
    ]
    & concat
  Check c successTrie failureTrie ->
    [ [ "if (*input == (char)" ++ show c ++ ") {"
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
      &. map (\(source, target) -> (utf8Encode source, utf8Encode target))
      &. constructTrie
      &. formatFullTrie

main :: IO ()
main = interact run
