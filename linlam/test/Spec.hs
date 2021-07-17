module Main where

import Syntax
import Parser
import Interpreter
import Checker

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [parseTests, checkerTests, evalTests]

parseTests :: TestTree
parseTests = testGroup "Parse tests"
  [ testCase "parse var single char" $
    (parseLL "x") @?= (TVar $ Var "x")
  , testCase "parse var multiple chars" $
    (parseLL "x0") @?= (TVar $ Var "x0")
  , testCase "parse var underscore" $
    (parseLL "_x") @?= (TVar $ Var "_x")
  , testCase "parse bool true unrestricted" $
    (parseLL "True[u]") @?= (TBool Unrestricted BTrue)
  , testCase "parse bool true linear" $
    (parseLL "True[l]") @?= (TBool Linear BTrue)
  , testCase "parse bool false unrestricted" $
    (parseLL "False[u]") @?= (TBool Unrestricted BFalse)
  , testCase "parse bool false linear" $
    (parseLL "False[l]") @?= (TBool Linear BFalse)
  , testCase "parse if" $
    (parseLL "if True[u] then False[u] else True[u]") @?=
    (TIf (TBool Unrestricted BTrue) (TBool Unrestricted BFalse) (TBool Unrestricted BTrue))
  , testCase "parse pair" $
    (parseLL "<True[u], False[l]>[u]") @?=
    (TPair Unrestricted (TBool Unrestricted BTrue) $ TBool Linear BFalse)
  , testCase "parse split" $
    (parseLL "split <True[u], False[l]>[u] as x,y in x") @?=
    (TSplit (TPair Unrestricted (TBool Unrestricted BTrue) (TBool Linear BFalse))
            (Var "x") (Var "y") (TVar $ Var "x"))
  , testCase "parse abs" $
    (parseLL "\\ [u] x: Bool[l]. x") @?=
    (TAbs Unrestricted (Var "x") (Type Linear PBool) $ TVar $ Var "x")
  , testCase "parse abs accepting func" $
    (parseLL "\\ [u] f: (Bool[u] -> Bool[u])[l]. apply f to True[u]") @?=
    (TAbs Unrestricted (Var "f") (Type Linear $ PFunc (Type Unrestricted PBool)
                                                      (Type Unrestricted PBool))
                                 (TApp (TVar $ Var "f") (TBool Unrestricted BTrue)))
  , testCase "parse app" $
    (parseLL "apply (\\ [u] x: Bool[l]. x) to True[u]") @?=
    (TApp (TAbs Unrestricted (Var "x") (Type Linear PBool) $ TVar $ Var "x")
     (TBool Unrestricted BTrue))
  , testCase "parse parens" $
    (parseLL "(True[l])") @?= (TBool Linear BTrue)
  ]

checkerTests :: TestTree
checkerTests = testGroup "Typechecker tests"
  [ testCase "check bool linear" $
    typecheck (TBool Linear BTrue) @?=
    Right (Type Linear PBool)
  , testCase "check bool unrestricted" $
    typecheck (TBool Unrestricted BTrue) @?=
    Right (Type Unrestricted PBool)
  , testCase "check if (illtyped b/c bad condition)" $
    typecheck (TIf (TPair Linear (TBool Linear BFalse) (TBool Unrestricted BTrue))
                   (TBool Linear BFalse)
                   (TBool Unrestricted BTrue)) @?=
    Left (IlltypedException "invalid condition")
  , testCase "check if (illtyped b/c unmatched branch types)" $
    typecheck (TIf (TBool Linear BFalse)
                   (TBool Linear BFalse)
                   (TBool Unrestricted BTrue)) @?=
    Left (IlltypedException "unmatched then/else branch types in if term")
  , testCase "check if (illtyped b/c same linear var used in cond and body)" $
    typecheck (TApp (TAbs Linear (Var "x") (Type Linear PBool)
                          (TIf (TVar $ Var "x")
                               (TVar $ Var "x")
                               (TBool Linear BTrue)))
                    (TBool Linear BFalse)) @?=
    Left (IlltypedException "variable not found")
  , testCase "check if (illtyped b/c same different contexts in then/else)" $
    typecheck (TApp (TAbs Linear (Var "x") (Type Linear PBool)
                          (TIf (TBool Unrestricted BFalse)
                               (TVar $ Var "x") -- uses/returns the linear bool argument
                               (TBool Linear BTrue))) -- doesn't use the linear argument, but returns a linear bool
                    (TBool Linear BFalse)) @?=
    Left (IlltypedException "unmatched final context in if term")
  , testCase "check pair unr containing unr" $
    typecheck (TPair Unrestricted (TBool Unrestricted BTrue) (TBool Unrestricted BFalse)) @?=
    Right (Type Unrestricted $ PPair (Type Unrestricted PBool) (Type Unrestricted PBool))
  , testCase "check pair lin containing unr" $
    typecheck (TPair Linear (TBool Unrestricted BTrue) (TBool Unrestricted BFalse)) @?=
    Right (Type Linear $ PPair (Type Unrestricted PBool) (Type Unrestricted PBool))
  , testCase "check pair lin containing lin" $
    typecheck (TPair Linear (TBool Linear BTrue) (TBool Linear BFalse)) @?=
    Right (Type Linear $ PPair (Type Linear PBool) (Type Linear PBool))
  , testCase "check pair unr containing lin (illtyped)" $
    typecheck (TPair Unrestricted (TBool Unrestricted BTrue) (TBool Linear BFalse)) @?=
    Left (IlltypedException "linear type(s) found in unrestricted type")
  , testCase "check split (illtyped b/c lin var unused)" $
    typecheck (TSplit (TPair Linear (TBool Linear BFalse) (TBool Unrestricted BTrue))
                             (Var "x") (Var "y")
                             (TVar $ Var "y")) @?=
    Left (IlltypedException "linear variable found in resulting contxt division")
  , testCase "check split (welltyped)" $
    typecheck (TSplit (TPair Linear (TBool Linear BFalse) (TBool Unrestricted BTrue))
                             (Var "x") (Var "y")
                             (TVar $ Var "x")) @?=
    Right (Type Linear PBool)
  , testCase "check abs (welltyped: unres func with unres parameter returning new lin bool)" $
    typecheck (TAbs Unrestricted (Var "x") (Type Unrestricted PBool)
                (TBool Linear BTrue)) @?=
    Right (Type Unrestricted (PFunc (Type Unrestricted PBool) (Type Linear PBool)))
  , testCase "check abs (illtyped: unres func with unused linear param)" $
    typecheck (TAbs Unrestricted (Var "x") (Type Linear PBool)
                (TBool Linear BTrue)) @?=
    Left (IlltypedException "input and output contexts of unrestricted function abstraction unequal")
  , testCase "check abs (welltyped: unres func with linear param)" $
    typecheck (TAbs Unrestricted (Var "x") (Type Linear PBool)
                (TVar $ Var "x")) @?=
    Right (Type Unrestricted (PFunc (Type Linear PBool) (Type Linear PBool)))
  , testCase "check app (illtyped: applied wrong argument type)" $
    typecheck (TApp (TAbs Unrestricted (Var "x") (Type Linear PBool) (TVar $ Var "x"))
                    (TPair Unrestricted (TBool Linear BFalse) (TBool Linear BFalse)))
               @?=
    Left (IlltypedException "argument does not match expected type")
  , testCase "check app (illtyped: func expects linear, got unres arg)" $
    typecheck (TApp (TAbs Unrestricted (Var "x") (Type Linear PBool) (TVar $ Var "x"))
                    (TBool Unrestricted BFalse))
               @?=
    Left (IlltypedException "argument does not match expected type")
  , testCase "check app (illtyped: invalid function in func pos of app)" $
    typecheck (TApp (TBool Linear BFalse)
                    (TBool Unrestricted BFalse))
               @?=
    Left (IlltypedException "invalid application")
  , testCase "check app (welltyped)" $
    typecheck (TApp (TAbs Unrestricted (Var "x") (Type Linear PBool) (TVar $ Var "x"))
                    (TBool Linear BFalse))
               @?=
    Right (Type Linear PBool)
  ]

-- These aren't necessarily well-typed, but are well-formed
evalTests :: TestTree
evalTests = testGroup "Evaluation tests"
  [ testCase "bool" $
    interpretProgram (TBool Unrestricted BTrue) @?=
    OVBool BTrue
  , testCase "pair" $
    interpretProgram (TPair Unrestricted (TBool Unrestricted BTrue) (TBool Linear BFalse)) @?=
    OVPair (OVBool BTrue) (OVBool BFalse)
  , testCase "if" $
    interpretProgram (TIf (TBool Unrestricted BTrue)
                      (TPair Linear (TBool Linear BFalse) (TBool Unrestricted BTrue))
                      (TBool Linear BFalse)) @?=
    OVPair (OVBool BFalse) (OVBool BTrue)
  , testCase "split" $
    interpretProgram (TSplit (TPair Linear (TBool Linear BFalse) (TBool Unrestricted BTrue))
                             (Var "x") (Var "y")
                             (TVar $ Var "x")) @?=
    OVBool BFalse
  , testCase "abs" $
    interpretProgram (TAbs Linear (Var "x") (Type Linear PBool) (TVar $ Var "x")) @?=
    OVAbs (Var "x") (Type Linear PBool) (TVar $ Var "x")
  , testCase "app" $
    interpretProgram (TApp (TAbs Linear (Var "x") (Type Linear PBool) (TVar $ Var "x"))
                           (TBool Linear BTrue)) @?=
    OVBool BTrue
  ]
