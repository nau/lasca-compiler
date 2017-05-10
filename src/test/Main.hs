import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import qualified Text.Megaparsec as Megaparsec
import Parser
import Syntax
import Infer
import Type

import Data.List
import Data.Ord

main = defaultMain tests

fromRight (Right a) = a

tests :: TestTree
tests = testGroup "Tests" [parserTests, typerTests]

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
        Case (LitPattern (BoolLit True)) (Literal (withMetaPos 1 24) (IntLit 1))])
  , testCase "Pattern matching" $
        parseExpr "match foo { | Person(0, name, \"God\", None, _) -> 1 | _ -> match false { | true -> 4 } }" @?= Right (
          Match (Ident "foo") [
            Case (ConstrPattern "Person" [LitPattern (IntLit 0),VarPattern "name",LitPattern (StringLit "God"),
                  ConstrPattern "None" [],WildcardPattern]) (Literal (withMetaPos 1 50) (IntLit 1)),
            Case WildcardPattern (
              Match (Literal emptyMeta (BoolLit False)) [
                Case (LitPattern (BoolLit True)) (Literal (withMetaPos 1 83) (IntLit 4))])
          ])
  ]

typerTests = testGroup "Typer tests"
  [
    testCase "Pattern matching" $
      parseAndInferExpr "match true { | true -> 1 | false -> 2 }" @?= Forall [] (TypeIdent "Int")
  ]

benchTests = testGroup "Bench" [testCase "gen10k" $ parseAndInferFile "src/main/lasca/gen5k.lasca"]

parseAndInferExpr str = let
    expr = fromRight $ parseExpr str
    Right (infered, _) = inferExpr (createGlobalContext [expr]) defaultTyenv expr
  in infered

parseAndInferFile fname = do
  p <- readFile "src/main/lasca/Prelude.lasca"
  let preludeExprs = fromRight $ parseToplevel p
  file <- readFile fname
  case parseToplevel file of
    Left err -> error $ Megaparsec.parseErrorPretty err
    Right ex -> do
      let exprs = preludeExprs ++ ex
      let typeEnv = typeCheck (createGlobalContext exprs) exprs
      print typeEnv
      1 @?= 1
