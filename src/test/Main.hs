import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Parser
import Syntax

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
  ]