import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Parser
import Syntax
import Type

import Data.List
import Data.Ord

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests]

parserTests = testGroup "Parser tests"
  [ testCase "Parse true" $
      parseExpr "true" @?= Right (Literal emptyMeta (BoolLit True))
  , testCase "Empty String" $
        parseExpr "\"\"" @?= Right (Literal emptyMeta (StringLit ""))
  , testCase "Character Escaping" $
      parseExpr "\"String\n\"" @?= Right (Literal emptyMeta (StringLit "String\n"))
  , testCase "String Interpolation" $
      parseExpr "\"Hello\\t \\\\\\$${ test123 + 1 }\"" @?= Right (Apply emptyMeta (Ident "concat") [
        Array [Literal emptyMeta $ StringLit "Hello\t \\$", 
               Apply emptyMeta (Ident "toString") [Apply (withMetaPos 1 25) (Ident "+") [Ident "test123", Literal (withMetaPos 1 27) (IntLit 1)]]]
      ])
  , testCase "Pattern matching" $
      parseExpr "match true { | true -> 1 }" @?= Right (Match (Literal emptyMeta (BoolLit True)) [
        Case (ConstPattern (BoolLit True)) (Literal (withMetaPos 1 24) (IntLit 1))])
  , testCase "Pattern matching" $
        parseExpr "match foo { | 0 -> 0 | Person(id, name) -> 1 | bar -> 2 | _ -> match false { | true -> 4 } }" @?= Right (
          Match (Ident "foo") [
            Case (ConstPattern (IntLit 0)) (Literal (withMetaPos 1 20) (IntLit 0)),
            Case (ConstrPattern (DataConst "Person" [Arg "id" (TypeIdent "Any"), Arg "name" (TypeIdent "Any")])) (Literal (withMetaPos 1 44) (IntLit 1)),
            Case (ConstrPattern (DataConst "bar" [])) (Literal (withMetaPos 1 55) (IntLit 2)),
            Case AnyPattern (
              Match (Literal emptyMeta  (BoolLit False)) [
                Case (ConstPattern (BoolLit True)) (Literal (withMetaPos 1 88) (IntLit 4))])])
  ]