{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Golden as G
import Test.Tasty.Program
import System.FilePath
import System.FilePath.Glob
import qualified Text.Megaparsec as Megaparsec
import Shelly (shelly, run)
--import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Lasca.Parser
import Lasca.Syntax
import Lasca.Infer
import Lasca.Type
import Lasca.Options

import Data.List
import Data.Ord
import Data.Foldable

default (T.Text)

main :: IO ()
main = do
  goldens <- foldMap mkGoldenTests examples
  defaultMain (testGroup "Tests" ([parserTests, typerTests] ++ goldens ++ compileTests))

fromRight (Right a) = a

parserTests = testGroup "Parser tests"
  [ testCase "Parse true" $
      parseExpr "true" @?= Right (Literal emptyMeta (BoolLit True))
  , testCase "Empty String" $
        parseExpr "\"\"" @?= Right (Literal emptyMeta (StringLit ""))
  , testCase "Character Escaping" $
      parseExpr "\"String\n\"" @?= Right (Literal emptyMeta (StringLit "String\n"))
  , testCase "String Interpolation" $
      parseExpr "\"Hello\\t \\\\\\$${ test123 + 1 }\"" @?= Right (Apply emptyMeta (Ident emptyMeta "concat") [
        Array emptyMeta [Literal emptyMeta $ StringLit "Hello\t \\$",
               Apply emptyMeta (Ident emptyMeta "toString") [Apply (withMetaPos 1 25) (Ident emptyMeta "+") [Ident emptyMeta "test123", Literal (withMetaPos 1 27) (IntLit 1)]]]
      ])
  , testCase "Pattern matching" $
      parseExpr "match true { true -> 1 }" @?= Right (Match emptyMeta (Literal emptyMeta (BoolLit True)) [
        Case (LitPattern (BoolLit True)) (Literal (withMetaPos 1 24) (IntLit 1))])
  , testCase "Pattern matching" $
        parseExpr "match foo { Person(0, name, \"God\", None, _) -> 1 _ -> match false { true -> 4 } }" @?= Right (
          Match emptyMeta (Ident emptyMeta "foo") [
            Case (ConstrPattern "Person" [LitPattern (IntLit 0),VarPattern "name",LitPattern (StringLit "God"),
                  ConstrPattern "None" [],WildcardPattern]) (Literal (withMetaPos 1 50) (IntLit 1)),
            Case WildcardPattern (
              Match emptyMeta (Literal emptyMeta (BoolLit False)) [
                Case (LitPattern (BoolLit True)) (Literal (withMetaPos 1 83) (IntLit 4))])
          ])
  ]

typerTests = testGroup "Typer tests"
  [
    testCase "Pattern matching" $
      parseAndInferExpr "match true { true -> 1 false -> 2 }" @?= (TypeIdent "Int")
  ]

data Mode = Dyn | Stat | Both
data Config = Script { name :: String, compMode :: Mode, arguments :: [T.Text] }

examples = [
    Script "array.lasca" Both [],
    Script "binarytrees.lasca" Both ["10"],
    Script "data.lasca" Both [],
    Script "dynamic.lasca" Dyn [],
    Script "Either.lasca" Both [],
    Script "factorial.lasca" Both ["15"],
    Script "hello.lasca" Both [],
    Script "lambda.lasca" Both [],
    Script "List.lasca" Both [],
    Script "Map.lasca" Both [],
    Script "nbody.lasca" Both ["50000"],
    Script "nbody2.lasca" Both ["50000"],
    Script "nbody3.lasca" Both ["50000"],
    Script "Option.lasca" Both [],
    Script "ski.lasca" Both []
  ]

prependPath path script = script { name = path </> (name script) }
withMode s m = s { compMode = m }

mkGoldenTests s@(Script path mode args) = do
    let testName = takeBaseName path
    let goldenPath = "src" </> "test" </> "golden" </> (replaceExtension path ".golden")
    let script = prependPath "examples" s
    let tests = case mode of
          Both -> [ goldenVsString testName goldenPath (action (script `withMode` Stat)),
                    goldenVsString testName goldenPath (action (script `withMode` Dyn))]
          _ -> [goldenVsString testName goldenPath (action script)]
    return tests
  where
      action (Script path mode args) = do
          let txtPath = T.pack path
          actual <- runLasca txtPath mode args
          let bs = E.encodeUtf8 actual
          return (LBS.fromStrict bs)

runLasca path mode args = shelly $ do
    let extraArgs = case args of
            [] -> []
            ars -> "--" : args
    case mode of
        Stat -> run "lasca" (["-e", "-O2", "--mode", "static", path] ++ extraArgs)
        Dyn -> run "lasca" (["-e", "-O2", "--mode", "dynamic", path] ++ extraArgs)
        Both -> do
            run "lasca" (["-e", "-O2", "--mode", "static", path] ++ extraArgs)
            run "lasca" (["-e", "-O2", "--mode", "dynamic", path] ++ extraArgs)

compileTests = [
        testProgram "Compile hello.lasca" "lasca" ["-O2", "examples/hello.lasca"] Nothing
    ]

benchTests = testGroup "Bench" [
        testCase "2 KLOC"  $ parseAndInferFile "examples/gen.lasca",
        testCase "10 KLOC" $ parseAndInferFile "examples/gen10k.lasca"
    ]

parseAndInferExpr str = let
    expr = fromRight $ parseExpr str
    Right (infered, _) = inferExpr (emptyCtx emptyLascaOpts) defaultTyenv expr
  in infered

parseAndInferFile fname = do
    p <- Prelude.readFile "examples/Prelude.lasca"
    let preludeExprs = fromRight $ parseToplevel p
    file <- Prelude.readFile fname
    case parseToplevel file of
        Left err -> error $ Megaparsec.parseErrorPretty err
        Right ex -> do
            let exprs = preludeExprs ++ ex
            let typeEnv = typeCheck (emptyCtx emptyLascaOpts) exprs
            print typeEnv
            True @?= True
