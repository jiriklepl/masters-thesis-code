{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.AST where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Data
import LLVM.AST.Global (Global(section))

takeAnnot :: Annot annot node -> annot
takeAnnot (Annot annot node) = annot

data Annotation annot node = Annot annot node deriving (Show, Data)
type Annot annot node = Annotation annot (node annot)

newtype Unit a = Unit [Annot a TopLevel]
               deriving (Show, Data)

data TopLevel a = TopSection Text [Annot a Section]
                | TopDecl (Annot a Decl)
                | TopProcedure (Annot a Procedure)
                deriving (Show, Data)

data Section a = SecDecl (Annot a Decl)
               | SecProcedure (Annot a Procedure)
               | SecDatum (Annot a Datum)
               | SecSpan (Annot a Expr) (Annot a Expr) [Annot a Section]
               deriving (Show, Data)

data Decl a = ImportDecl [Annot a Import] -- at least one
            | ExportDecl [Annot a Export] -- at least one
            | ConstDecl (Maybe (Annot a Type)) Name (Annot a Expr)
            | TypedefDecl (Annot a Type) [Name] -- at least one
            | RegDecl (Maybe Invariant) (Annot a Registers)
            | PragmaDecl Name (Annot a Pragma)
            | TargetDecl [Annot a TargetDirective]
            deriving (Show, Data)

data TargetDirective a = MemSize Int
                       | ByteOrder Endian
                       | PointerSize Int
                       | WordSize Int
                       deriving (Show, Data)

data Endian = Little | Big
            deriving (Show, Data)

data Import a = Import (Maybe Text) Name
              deriving (Show, Data)

data Export a = Export Name (Maybe Text)
              deriving (Show, Data)

data Invariant = Invariant -- TODO: or it can be just Bool?
               deriving (Show, Data)

data Datum a = DatumLabel Name
             | DatumAlign Int
             | Datum (Annot a Type) (Maybe (Annot a Size)) (Maybe (Annot a Init))
             deriving (Show, Data)

data Init a = ExprInit [Annot a Expr]
            | StrInit Text
            | Str16Init String16
            deriving (Show, Data)

data Registers a = Registers (Maybe Kind) (Annot a Type) [(Name, Maybe Text)] -- at least one
                 deriving (Show, Data)

newtype Size a = Size (Maybe (Annot a Expr))
               deriving (Show, Data)

newtype Body a = Body [Annot a BodyItem]
               deriving (Show, Data)

data BodyItem a = BodyDecl (Annot a Decl)
                | BodyStackDecl (Annot a StackDecl)
                | BodyStmt (Annot a Stmt)
                deriving (Show, Data)

data Procedure a = Procedure (Maybe Conv) Name [Annot a Formal] (Annot a Body)
                 deriving (Show, Data)

data Formal a = Formal (Maybe Kind) (Maybe Invariant) (Annot a Type) Name
              deriving (Show, Data)

data Actual a = Actual (Maybe Kind) (Annot a Expr)
              deriving (Show, Data)

newtype Kind = Kind Text
             deriving (Show, Data)

data KindName a = KindName (Maybe Kind) Name
                deriving (Show, Data)

newtype StackDecl a = StackDecl [Annot a Datum] -- at least one
                    deriving (Show, Data)

data Stmt a = EmptyStmt
            | IfStmt (Annot a Expr) (Annot a Body) (Maybe (Annot a Body))
            | SwitchStmt (Annot a Expr) [Annot a Arm]
            | SpanStmt (Annot a Expr) (Annot a Expr) (Annot a Body)
            | AssignStmt [Annot a LValue] [Annot a Expr] -- TODO?: maybe list of tuples
            | PrimOpStmt Name Name [Annot a Actual] [Annot a Flow]
            | CallStmt [Annot a KindName] (Maybe Conv) (Annot a Expr) [Annot a Actual] (Maybe (Annot a Targets)) [Either (Annot a Flow) (Annot a Alias)]
            | JumpStmt (Maybe Conv) (Annot a Expr)  [Annot a Actual] (Maybe (Annot a Targets))
            | ReturnStmt (Maybe Conv) (Maybe (Annot a Expr, Annot a Expr)) [Annot a Actual]
            | LabelStmt Name
            | ContStmt Name [Annot a KindName]
            | GotoStmt (Annot a Expr) (Maybe (Annot a Targets))
            | CutToStmt (Annot a Expr) [Annot a Actual] [Annot a Flow]
            deriving (Show, Data)

data Arm a = Arm [Annot a Range] (Annot a Body)
           deriving (Show, Data)

data Range a = Range (Annot a Expr) (Maybe (Annot a Expr))
             deriving (Show, Data)

data LValue a = LVName Name -- TODO?: name
              | LVRef (Annot a Type) (Annot a Expr) (Maybe (Annot a Asserts)) -- TODO?: name
              deriving (Show, Data)

data Flow a = AlsoCutsTo [Name] -- at least one
            | AlsoUnwindsTo [Name] -- at least one
            | AlsoReturnsTo [Name] -- at least one
            | AlsoAborts
            | NeverReturns
            deriving (Show, Data)
-- TODO: should be a part of inference?

data Alias a = Reads [Name]
             | Writes [Name]
             deriving (Show, Data)

newtype Targets a = Targets [Name]
                  deriving (Show, Data)

data Expr a = LitExpr (Annot a Lit) (Maybe (Annot a Type))
            | NameExpr Name
            | RefExpr (Annot a Type) (Annot a Expr) (Maybe (Annot a Asserts))
            | ParExpr (Annot a Expr)
            | BinOpExpr Op (Annot a Expr) (Annot a Expr)
            | ComExpr (Annot a Expr)
            | NegExpr (Annot a Expr)
            | InfixExpr (Annot a Expr) Name (Annot a Expr)
            | PrefixExpr Name [Annot a Actual]
            deriving (Show, Data)

data Lit a = LitInt Int -- TODO?: think more
           | LitFloat Float
           | LitChar Char
           deriving (Show, Data)

data Type a = TBits Int -- TODO?: name
            | TName Name -- TODO?: name
            deriving (Show, Data)

newtype String16 = String16 Text -- TODO: implement String16
                 deriving (Show, Data)

newtype Conv = Foreign Text
             deriving (Show, Data)

data Asserts a = AlignAssert Int [Name]
               | InAssert [Name] (Maybe Int)
               deriving (Show, Data)

data Op = AddOp | SubOp | MulOp | DivOp | ModOp | AndOp | OrOp | XorOp | ShLOp | ShROp | EqOp | NeqOp | GtOp | LtOp | GeOp | LeOp
        deriving (Show, Data)

newtype Name = Name Text -- TODO: consult this with the manual (and supervisor) (and also add hash etc?)
             deriving (Show, Data)

data Pragma a deriving (Data) -- TODO: the manual does not specify at all

instance Show (Pragma a) where
    show = undefined -- TODO
