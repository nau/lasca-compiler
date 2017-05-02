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
  , testCase "Empty String Interpolator" $
        parseInterpol "\"\"" @?= Right []
  , testCase "Character Escaping" $
      parseExpr "\"String\n\"" @?= Right (Literal emptyMeta (StringLit "String\n"))
  , testCase "String Interpolation" $
      parseInterpol "\"Hello\\t \\\\\\$${ test123 + 1 }\"" @?= Right [Left "Hello\t \\$",
        Right "Apply Meta{pos=1:25, tpe=Any} (Ident \"+\") [Ident \"test123\",Literal Meta{pos=1:27, tpe=Any} (IntLit 1)]"]
  ]