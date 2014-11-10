-- | An abstract syntax tree definition for the Fast language.
module Fast.FastAST
       ( Term (..)
       , ObjectReference
       , Value (..)
       , Name
       , Expr (..)
       , Exprs
       , Cases
       , Case
       , Pattern (..)
       , ClassDecl (..)
       , ConstructorDecl
       , NamedMethodDecl (..)
       , ReceiveDecl (..)
       , MethodDecl (..)
       , Prog
       )
       where

-- | A name is a string.
type Name = String

-- | A term is an identifying symbol (the "tag") followed by a
-- possibly non-empty sequence of values.
data Term = Term Name [Value]
            deriving (Eq, Show)

-- | An object reference is an integer uniquely identifying an object.
-- This does not appear in the grammar, but is used in the runtime
-- representation in the interpreter.
type ObjectReference = Int

-- | A value is either a term, an integer, or a string.  Expressions
-- are evaluated to values and methods return values.
data Value = TermValue Term
           | IntValue Integer
           | StringValue String
           | ReferenceValue ObjectReference
           deriving (Eq, Show)

-- | A Fast expression.  May execute slowly.
data Expr = IntConst Integer
          | StringConst String
          | TermLiteral Name [Expr]
          | Self
          | Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | DividedBy Expr Expr
          | Return Expr
          | SetField Name Expr
          | SetVar Name Expr
          | ReadVar Name
          | ReadField Name
          | Match Expr Cases
          | SendMessage
            Expr -- ^ Receiver
            Expr -- ^ The message
          | CallMethod
            Expr -- ^ Receiver
            Name -- ^ Method name
            [Expr] -- ^ Method arguments
          | New Name [Expr]
          deriving (Eq, Show)

type Exprs = [Expr]

type Cases = [Case]

type Case = (Pattern,Exprs)

data Pattern = ConstInt Integer
             | ConstString String
             | TermPattern Name [Name]
             | AnyValue Name
             deriving (Eq, Show)

data ClassDecl = ClassDecl { className :: Name
                           , classConstructor :: Maybe ConstructorDecl
                           , classMethods :: [NamedMethodDecl]
                           , classReceive :: Maybe ReceiveDecl
                           }
               deriving (Eq, Show)

type ConstructorDecl = MethodDecl

data ReceiveDecl = ReceiveDecl { receiveParam :: Name
                               , receiveBody :: Exprs
                               }
                 deriving (Eq, Show)

data NamedMethodDecl = NamedMethodDecl Name MethodDecl
                     deriving (Eq, Show)

data MethodDecl = MethodDecl { methodParameters :: [Name]
                             , methodBody :: Exprs
                             }
                deriving (Eq, Show)

-- | A program is just a list of class declarations.  The Fast program
-- is executed by creating an instance of a class named Main.  If
-- there is no such class defined, interpretation must fail.
type Prog = [ClassDecl]
