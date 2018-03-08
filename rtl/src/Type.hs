module Type where

import Data.Set as S
import Data.Map as M
import AST

type Environment = M.Map Var Type
newtype EffectVar = EffectVar String deriving (Eq, Ord, Show)
type Effect = S.Set (Either Place EffectVar)
type EffectType = (Type, Effect)

-- | Types
data Type
  = TyVar Var
    -- ^ Type variable @X@

  | TyBool
    -- ^ Boolean @bool@

  | TyFun (Type, Type, Effect) Place
    -- ^ Function type @(T → φT, p)@

  | TyRegFun (RegionVar, Type, Effect) Place
    -- ^ Region function @(Πρ.φT, p)@

  | TyPoly Var Type
    -- ^ Type polymorphism @∀X.T@

  | TyEffectPoly EffectVar Type
    -- ^ Effect polymorphism @∀ϵ.T@

  deriving (Eq, Ord, Show)

typecheck :: Environment -> Term -> EffectType
typecheck env t = case t of

  TmAlmost a -> case a of
    AValue v -> case v of
      VBool _ -> (TyBool, S.empty)                  -- RT-Bool
      VClosure x t p -> error "TODO RT-Clos"        -- RT-Clos
      VRegionClosure r t p -> error "TODO RT-RClos" -- RT-RClos
    AAbs x t p -> error "TODO RT-Abs"               -- RT-Abs
    ARegionAbs r u p -> error "TODO RT-RAbs"        -- RT-RAbs

  TmVar v -> case M.lookup v env of
    Nothing -> error $ "var " ++ show v ++ " not found"
    Just ty -> (ty, S.empty)                        -- RT-Var

  {-
  TmIf t1 t2 t3 ->
    let (t1Ty, ef1) = typecheck env t1
        (t2Ty, ef2) = typecheck env t2
        (t3Ty, ef3) = typecheck env t3
    in case t1Ty of
      TyBool | all (== ef1) [ef2, ef3]
             && t2Ty == t3Ty -> (t2Ty, ef2)
      _ -> error "invalid if"
  -}
