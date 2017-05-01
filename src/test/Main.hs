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
  , testCase "String Interpolation" $
      parseExpr "\"String\n\"" @?= Right (Literal emptyMeta (StringLit "String\n"))
  ]