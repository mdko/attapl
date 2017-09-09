module Checker (typecheck, LLException(..)) where

import Syntax
import Control.Monad.Trans.State.Lazy
import Control.Exception (Exception, throw, try)
import Control.Monad.Except

type LS a = ExceptT LLException (State Context) a

data LLException = IlltypedException String
instance Show LLException where
  show (IlltypedException s) = "illtyped: " ++ s
instance Eq LLException where
  IlltypedException _ == IlltypedException _ = True
instance Exception LLException

typecheck :: Term -> Either LLException Type
typecheck t = evalState (runExceptT $ check t) []

getCtx :: LS Context
getCtx = lift $ get

setCtx :: Context -> LS ()
setCtx = lift . put

check :: Term -> LS Type
check (TVar x) = do
  ctx <- getCtx
  case lookup x ctx of
    Nothing -> throwError $ IlltypedException "variable not found"
    Just t@(Type q p) -> case q of
      Linear -> do
        setCtx $ filter (\(v, _) -> v /= x) ctx -- Using linear variable, so remove from context
        return t
      Unrestricted -> return t
check (TBool q b) = return $ Type q PBool
check (TIf t1 t2 t3) = do
  typ1 <- check t1
  case typ1 of
    Type q PBool -> do
      -- typecheck t2
      ctx2 <- getCtx
      typ2 <- check t2
      ctx3 <- getCtx
      -- typecheck t3 with same starting context used for t2
      setCtx ctx2
      typ3 <- check t3
      ctx3' <- getCtx
      -- make sure both contexts returned in checking t2 and t3 are same
      if (ctx3 /= ctx3')
      then throwError $ IlltypedException "unmatched final contexts in if term"
      else if (typ2 /= typ3)
      then throwError $ IlltypedException "unmatched then/else branch types in if term"
      else return typ3
    _ -> throwError $ IlltypedException "invalid condition"
check (TPair q t1 t2) = do
  typ1 <- check t1
  typ2 <- check t2
  if (not (qqual q typ1) || not (qqual q  typ2))
  then throwError $ IlltypedException "linear type(s) found in unrestricted type"
  else return $ Type q $ PPair typ1 typ2
check (TSplit t1 x y t2) = do
  typ1 <- check t1
  case typ1 of
    Type q (PPair typx typy) -> do
      ctx1 <- getCtx
      let ext = [(x, typx), (y, typy)]
      setCtx $ ctx1 ++ ext
      typ2 <- check t2
      ctx2 <- getCtx
      setCtx <$> ctx2 `ctxDiv` ext -- Note: using 'fmap' (i.e. <$>) is same as '=<<' here b/c (()) gets flattened to ()
      return typ2
    _ -> throwError $ IlltypedException "invalid type to split on"
check (TAbs q x typ1 t2) = do
  ctx1 <- getCtx
  setCtx $ ctx1 ++ [(x, typ1)]
  typ2 <- check t2
  ctx2 <- getCtx
  if (q == Unrestricted && ctx1 /= ctx2)
  then throwError $ IlltypedException "input and output contexts of unrestricted function abstraction unequal"
  else do
    setCtx <$> ctx2 `ctxDiv` [(x, typ1)]
    return $ Type q $ PFunc typ1 typ2
check (TApp t1 t2) = do
  typ1 <- check t1
  case typ1 of
    Type _ (PFunc typ11 typ12) -> do
      typ11' <- check t2
      if (typ11 /= typ11')
      then throwError $ IlltypedException "argument does not match expected type"
      else return typ12
    _ -> throwError $ IlltypedException "invalid application"

qqual :: Qualifier -> Type -> Bool
qqual q (Type q' p) = q <= q'

ctxDiv :: Context -> Context -> LS Context
ctxDiv ctx [] = return ctx
ctxDiv ctx1 ((x, Type Linear p):ctx2) = do
  ctx3 <- ctx1 `ctxDiv` ctx2
  case (lookup x ctx3) of
    Nothing -> return ctx3
    Just _ -> throwError $ IlltypedException "linear variable found in resulting context division"
ctxDiv ctx1 ((x, Type Unrestricted p):ctx2) = do
  ctx3 <- ctx1 `ctxDiv` ctx2
  return $ filter (\(v, _) -> x /= v) ctx3
