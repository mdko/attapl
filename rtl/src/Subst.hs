module Subst where

import AST

-- Capture-avoiding substitution.
-- Replace x with t1 in t2
subst :: Var -> Term -> Term -> Term
subst x t1 t2 = case t2 of
  TmAlmost a -> TmAlmost $ substA x t1 a
  TmVar y | x == y -> t1
  TmIf t1 t2 t3 -> TmIf (f t1) (f t2) (f t3)
  TmFix x u -> TmFix x $ substA x t1 u
  TmApp t1 t2 -> TmApp (f t1) (f t2)
  TmRegionApp t p -> TmRegionApp (f t) p
  TmRegionNew r1 t -> TmRegionNew r1 $ f t
  where f = subst x t1

substA :: Var -> Term -> AlmostValue -> AlmostValue
substA x t a = case a of
  AValue v -> AValue $ substV x t v
  AAbs y t2 p | x /= y -> AAbs y (subst x t t2) p
  ARegionAbs r u p -> ARegionAbs r (substA x t u) p

substV :: Var -> Term -> Value -> Value
substV x t v = case v of
  VBool _ -> v
  VClosure x t2 p -> VClosure x (subst x t t2) p
  VRegionClosure r u p -> VRegionClosure r (substA x t u) p

-- Replace p1 with p2 in t
substTPlace :: Place -> Place -> Term -> Term
substTPlace p1 p2 t = case t of
  TmAlmost u -> TmAlmost $ substAPlace p1 p2 u
  TmVar _ -> t
  TmIf t1 t2 t3 -> TmIf (f t1) (f t2) (f t3)
  TmFix x u -> TmFix x $ substAPlace p1 p2 u
  TmApp t1 t2 -> TmApp (f t1) (f t2)
  TmRegionApp t p -> TmRegionApp (f t) $ substPlace p1 p2 p
  TmRegionNew r1 t | (PVar r2) <- p1 -- Pattern guards FTW!
                   , r1 /= r2 -> TmRegionNew r1 $ substTPlace p1 p2 t
                   | otherwise -> t
  where f = substTPlace p1 p2

substAPlace :: Place -> Place -> AlmostValue -> AlmostValue
substAPlace p1 p2 a = case a of
  AValue v -> AValue $ substVPlace p1 p2 v
  AAbs y t p -> AAbs y (substTPlace p1 p2 t) (substPlace p1 p2 p)
  ARegionAbs r u p -> ARegionAbs r (substAPlace p1 p2 u) (substPlace p1 p2 p)

substVPlace :: Place -> Place -> Value -> Value
substVPlace p1 p2 v = case v of
  VBool _ -> v
  VClosure x t p -> VClosure x (substTPlace p1 p2 t) (substPlace p1 p2 p)
  VRegionClosure r u p | p /= p1 -> VRegionClosure r (substAPlace p1 p2 u) (substPlace p1 p2 p)
                       | otherwise -> v

substPlace :: Place -> Place -> Place -> Place
substPlace from to loc | from == loc = to
                        | otherwise = loc
