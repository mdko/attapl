module Syntax where

data Qualifier
  = Linear
  | Unrestricted
  deriving (Show, Eq)

instance Ord Qualifier where
  (<=) Unrestricted Linear = False
  (<=) _ _ = True

data Boolean
  = BTrue
  | BFalse
  deriving (Show, Eq)

newtype Var = Var { getVar :: String } deriving (Show, Eq, Ord)

-- TODO extend with a let (which is just sugar): let x = t1 in t2 <=> (\x.t2) t1
data Term
  = TVar Var
  | TBool Qualifier Boolean
  | TIf Term Term Term
  | TPair Qualifier Term Term
  | TSplit Term Var Var Term
  | TAbs Qualifier Var Type Term
  | TApp Term Term
  deriving (Show, Eq)

data Pretype
  = PBool
  | PPair Type Type
  | PFunc Type Type
  deriving (Show, Eq)

data Type = Type Qualifier Pretype deriving (Show, Eq)

-- An association list (not a map to emphasize order, in case this is extended for ordered LC)
type Context = [(Var, Type)]
