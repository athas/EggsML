module Fast.FastParser
       ( Error
       , parseString
       , parseFile
       )
       where

import Fast.FastAST

import Data.Char
import Control.Monad
import Control.Applicative
import Text.Parsec hiding (digit, (<|>), many, token, optional)
import Text.Parsec.String

token :: Parser a -> Parser a
token p = do x <- p
             spaces
             return x

symbol :: String -> Parser ()
symbol = try . void . token . string

schar :: Char -> Parser ()
schar = void . token . char

constituent :: Char -> Bool
constituent c = isAlphaNum c || c == '_'

keywords :: [String]
keywords = ["self", "class", "new", "receive",
            "send", "match", "return", "set"]

name :: Parser String
name = try $ token $ do s <- liftM2 (:) (satisfy isLetter) (many $ satisfy constituent)
                        when (s `elem` keywords) $
                          fail $ "Found keyword '" ++ s ++ "'"
                        return s

digit :: Parser Integer
digit = num <$> satisfy isDigit <?> "digit"
  where num c = toInteger $ ord c - ord '0'

intconst :: Parser Integer
intconst = token $ foldl (\acc x -> acc * 10 + x) 0 <$> some digit

stringconst :: Parser String
stringconst = token $ do _ <- char '"'
                         s <- many $ satisfy (/='"')
                         _ <- char '"'
                         return s

parens :: Parser a -> Parser a
parens = between (schar '(') (schar ')')

braces :: Parser a -> Parser a
braces = between (schar '{') (schar '}')

termliteral :: Parser Expr
termliteral = do s <- name
                 es <- parens $ expr `sepBy` schar ','
                 return $ TermLiteral s es

sendMessage :: Parser Expr
sendMessage = do
  symbol "send"
  parens $ do
    dest <- expr
    schar ','
    msg <- expr
    return $ SendMessage dest msg

caseMatch :: Parser Expr
caseMatch = do
  symbol "match"
  e <- expr
  cases <- braces $ many matchCase
  return $ Match e cases

matchCase :: Parser (Pattern,Exprs)
matchCase = do
  pat <- pattern
  symbol "->"
  es <- braces exprs
  return (pat, es)

pattern :: Parser Pattern
pattern = liftM ConstInt intconst <|>
          liftM ConstString stringconst <|>
          try (liftM2 TermPattern name (parens $ name `sepBy` schar ',')) <|>
          liftM AnyValue name

prim :: Parser Expr
prim = liftM IntConst intconst <|>
       liftM StringConst stringconst <|>
       try termliteral <|>
       liftM ReadVar name <|>
       sendMessage <|>
       caseMatch <|>
       (symbol "return" >> liftM Return expr) <|>
       create <|>
       assignment <|>
       (symbol "self" >> return Self) <|>
       parens expr

create :: Parser Expr
create = do
  symbol "new"
  cname <- name
  args <- parens $ expr `sepBy` schar ','
  return $ New cname args

assignment :: Parser Expr
assignment = symbol "set" >> (varassign <|> fieldassign)
  where varassign = do
          var <- name
          symbol "="
          e <- expr
          return $ SetVar var e
        fieldassign = do
          symbol "self"
          symbol "."
          field <- name
          symbol "="
          e <- expr
          return $ SetField field e

expr :: Parser Expr
expr = chainl1 e0 op0
  where e0 = chainl1 e1 op1
        e1 = do e <- prim
                methodCall e
        methodCall src = do
          schar '.'
          methodname <- name
          callMethod src methodname
          <|> return src
        callMethod src fieldname = do
          args <- parens $ expr `sepBy` schar ','
          methodCall $ CallMethod src fieldname args
          <|> methodCall (ReadField fieldname)

op0 :: Parser (Expr -> Expr -> Expr)
op0 = (do _ <- symbol "+"
          return Plus) <|>
      (do _ <- symbol "-"
          return Minus)

op1 :: Parser (Expr -> Expr -> Expr)
op1 = (do _ <- symbol "/"
          return DividedBy) <|>
      (do _ <- symbol "*"
          return Times)

exprs :: Parser Exprs
exprs = many $ expr <* schar ';'

constructor :: Parser ConstructorDecl
constructor = do
  symbol "new"
  params <- parens $ name `sepBy` schar ','
  braces $ do
    body <- exprs
    return $ MethodDecl params body

method :: Parser NamedMethodDecl
method = do
  methodname <- name
  params <- parens $ name `sepBy` schar ','
  braces $ do
    body <- exprs
    return $ NamedMethodDecl methodname $ MethodDecl params body

receive :: Parser ReceiveDecl
receive = do
  symbol "receive"
  param <- parens name
  braces $ do
    body <- exprs
    return $ ReceiveDecl param body

classDecl :: Parser ClassDecl
classDecl = do symbol "class"
               classname <- name
               braces $ do
                 cons <- liftM Just constructor <|> return Nothing
                 methods <- many method
                 recv <- liftM Just receive <|> return Nothing
                 return $ ClassDecl classname cons methods recv

commentline :: Parser ()
commentline = void $ char '#' >> many (noneOf "\n") >> char '\n'

prog :: Parser Prog
prog = do _ <- many commentline
          spaces
          res <- many classDecl
          eof
          return res

-- | You may change this type to whatever you want - just make sure it
-- is an instance of 'Show'.
type Error = ParseError

parseString :: String -> Either Error Prog
parseString = parse prog "input"

parseFile :: FilePath -> IO (Either Error Prog)
parseFile = liftM parseString . readFile
