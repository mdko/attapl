module AST where

newtype Var = Var String deriving (Eq, Ord, Show)
newtype RegionVar = RegionVar String deriving (Eq, Ord, Show)

-- | Terms
data Term
  = TmAlmost AlmostValue
    -- ^ value or almost-value @u@

  | TmVar Var
    -- ^ variable @x@

  | TmIf Term Term Term
    -- ^ conditional @if t then t else t@

  | TmFix Var AlmostValue
    -- ^ recursion @fix x.u@

  | TmApp Term Term
    -- ^ application @t t@

  | TmRegionApp Term Place
    -- ^ region application @t [[p]]@

  | TmRegionNew RegionVar Term
    -- ^ region creation @new ρ.t@

-- | Almost-values
data AlmostValue
  = AValue Value
    -- ^ Value @v@

  | AAbs Var Term Place
    -- ^ Abstraction @(λx.t) at p@

  | ARegionAbs RegionVar AlmostValue Place
    -- ^ Region Abstraction @(λρ.u) at p@

-- | Value Expressions
data Value
  = VBool Bool
    -- ^ Truth value @bv@

  | VClosure Var Term Place
    -- Closure @〈λx.t〉_p@

  | VRegionClosure RegionVar AlmostValue Place
    -- Region Closure @〈λρ.t〉_p@

data Place
  = PVar RegionVar
    -- ^ Region variable @ρ@

  | PDeallocated
    -- ^ Deallocated @∙@
  deriving (Eq, Ord, Show)
