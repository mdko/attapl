module Eval where

import AST
import Subst

eval :: Term -> Term
eval t = case t of

  TmAlmost a -> case a of
    AValue _ -> t
    AAbs x t p -> TmAlmost $ AValue $ VClosure x t p
    ARegionAbs r1 u r2 -> TmAlmost $ AValue $ VRegionClosure r1 u r2

  TmVar v -> error "variable, will work once runtime configuration added"

  TmIf t1 t2 t3 -> case (eval t1) of
    TmAlmost (AValue (VBool True)) -> eval t2
    TmAlmost (AValue (VBool False)) -> eval t3

  -- Note: Some good subtyping would be mighty nice to
  --       avoid having to wrap u in a TmAlmost...
  TmFix x u -> case eval (TmAlmost u) of
    v@(TmAlmost av@(AValue _)) -> subst x (TmFix x av) v
    _ -> error "recursion"

  TmApp t1 t2 -> case eval t1 of
    TmAlmost (AValue (VClosure x t p)) -> case eval t2 of
      v@(TmAlmost (AValue _)) -> subst x v t
      _ -> error "application: bad argument"
    _ -> error "application: bad function"

  TmRegionApp t p -> case eval t of
    TmAlmost (AValue (VRegionClosure p1 u _)) -> TmAlmost $ substAPlace (PVar p1) p u
    _ -> error "region application"

  -- TODO extend with State monad so we can track region environment and store, actually allocate a region that can be used
  TmRegionNew r t -> case eval t of
    v@(TmAlmost (AValue _)) -> substTPlace (PVar r) PDeallocated t
