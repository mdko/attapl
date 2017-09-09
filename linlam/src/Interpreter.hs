module Interpreter (interpretProgram, OutputValue(..)) where

import Syntax

import Data.Map as M
import Control.Monad.Trans.State.Lazy
import Data.Either.Utils (maybeToEither)
import Control.Monad (liftM)

data LLState
  = LLState
  { store :: Store
  , nextVar :: Int
  }

type LL = State LLState

data Prevalue
  = VBool Boolean
  | VPair Var Var
  | VAbs Var Type Term
  deriving (Show)

data Value = Value Qualifier Prevalue deriving (Show)

-- Remove all variables in normal forms, for easy comparison/outputting to outside world
data OutputValue
  = OVBool Boolean
  | OVPair OutputValue OutputValue
  | OVAbs Var Type Term
  deriving (Eq)

instance Show OutputValue where
  show (OVBool BTrue) = "True"
  show (OVBool BFalse) = "False"
  show (OVPair v1 v2) = "<" ++ show v1 ++ "," ++ show v2 ++ ">"
  show (OVAbs x typ t) = "\\" ++ show x ++ ":" ++ show typ ++ "." ++ show t

-- Note: so the use of evaluation contexts in a semantics is just a shorthand way of explaining
--       how you would do evaluation using a normal judgment form...
--       I decided to emulate evaluation contexts in *some* of the check functions below, just
--       to see the difference in implementation effor/clarity

type Store = M.Map Var Value

interpretProgram :: Term -> OutputValue
interpretProgram t = evalState (interpretToValue t) (LLState M.empty 0)

toOutputValue :: Value -> LL OutputValue
toOutputValue (Value _ (VBool b)) = return $ OVBool b
toOutputValue (Value _ (VPair x y)) = do
  mv1 <- stLookup x
  mv2 <- stLookup y
  case (mv1, mv2) of
    (Just val1, Just val2) -> do
      v1 <- toOutputValue val1
      v2 <- toOutputValue val2
      return $ OVPair v1 v2
    _ -> error "invalid variables in pair value"
toOutputValue (Value _ (VAbs v typ t)) = return $ OVAbs v typ t

interpretToValue :: Term -> LL OutputValue
interpretToValue t = do
  finalTerm <- interpret t
  case finalTerm of
    TVar x -> do
      mv <- stLookup x
      case mv of
        Nothing -> error "final variable term not found in store"
        Just v -> toOutputValue v
    o -> error $ "final term should be a variable: " ++ show o

interpret :: Term -> LL Term
interpret t@(TVar _) = return t
interpret (TBool q b) = do
  v <- freshVar
  extendStore v $ Value q $ VBool b
  return $ TVar v

interpret (TIf (TVar x) t2 t3) = do
  val <- stLookup x
  case val of
    Nothing -> error "unknown variable"
    Just v -> case v of
      Value q (VBool BTrue) -> do
        dropStore q x
        interpret t2
      Value q (VBool BFalse) -> do
        dropStore q x
        interpret t3
      _ -> error "invalid condition"
interpret (TIf e t2 t3) = do
  t1 <- interpret e
  interpret (TIf t1 t2 t3)

-- *Kind-of* in the spirit of evaluation contexts
-- could also do just case on the result of interpreting
-- each term, in order, within one body...
interpret (TPair q (TVar x) (TVar y)) = do
  v <- freshVar
  extendStore v $ Value q $ VPair x y
  return $ TVar v
interpret (TPair q (TVar x) e) = do
  t2 <- interpret e
  interpret (TPair q (TVar x) t2)
interpret (TPair q e t) = do
  t1 <- interpret e
  interpret (TPair q t1 t)

-- ...like this:
interpret (TSplit t1 y z t2) = do
  t1' <- interpret t1
  case t1' of
    TVar x -> do
      v <- stLookup x
      case v of
        Just (Value q (VPair y' z')) -> do
          dropStore q x
          interpret $ replace y y' $ replace z z' t2
        _ -> error "invalid value to split on"
    _ -> error "invalid term in first position of split"

interpret (TAbs q v typ t) = do
  x <- freshVar
  extendStore x $ Value q $ VAbs v typ t
  return $ TVar x

interpret (TApp (TVar x1) (TVar x2)) = do
  v <- stLookup x1
  case v of
    Nothing -> error "invalid variable in function position of app"
    Just (Value q (VAbs v typ t)) -> do
      dropStore q x1
      interpret $ replace v x2 t
interpret (TApp (TVar x) t) = do
  y <- interpret t
  interpret $ TApp (TVar x) y
interpret (TApp t1 t2) = do
  x <- interpret t1
  interpret $ TApp x t2

replace :: Var -> Var -> Term -> Term
replace x x' t = case t of
  TVar v | x == v -> TVar x'
  b@(TBool _ _) -> b
  TIf t1 t2 t3 -> TIf (replace x x' t1) (replace x x' t2) (replace x x' t3)
  TPair q t1 t2 -> TPair q (replace x x' t1) (replace x x' t2)
  TSplit t1 v1 v2 t2 -> TSplit (replace x x' t1) v1 v2 (replace x x' t2)
  TAbs q v typ t ->
    let tNew = if x == v then t {- shadowing -} else replace x x' t
    in TAbs q v typ tNew
  TApp t1 t2 -> TApp (replace x x' t1) (replace x x' t2)
  _ -> t

freshVar :: LL Var
freshVar = do
  st <- get
  let next = nextVar st
  put $ st { nextVar = next + 1 }
  return $ Var $ "__" ++ show next

extendStore :: Var -> Value -> LL ()
extendStore x v = do
  st <- get
  let str = store st
  put $ st { store = M.insert x v str }
  return ()

stLookup :: Var -> LL (Maybe Value)
stLookup x = do
  st <- get
  let str = store st
  return $ M.lookup x str

-- Possibly drops a key-value pair from the store,
-- depending on the qualifier
dropStore :: Qualifier -> Var -> LL ()
dropStore q x =
  if q == Unrestricted
  then return ()
  else do
    st <- get
    let str = store st
    put $ st { store = M.delete x str }
    return ()
