{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Golden as G
import Test.Tasty.Program
import System.FilePath
import System.FilePath.Glob
import System.Directory
import Control.Exception as X
import qualified Text.Megaparsec as Megaparsec
import Shelly (shelly, run)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Lasca.Parser
import Lasca.Syntax
import Lasca.Infer
import Lasca.Type
import Lasca.Options
import Lasca.Modules

import Data.List
import Data.Ord
import Data.Foldable

default (T.Text)

main :: IO ()
main = do
  goldens <- foldMap mkGoldenTests examples
  defaultMain (testGroup "Tests" ([parserTests, parserTests2,  modulesTests, typerTests] ++ goldens ++ compileTests))

modul n is = LascaModule { imports = is, moduleExprs = [], modName = n }

prelude = modul "Prelude" []
arr = modul "Array" [prelude]
opt = modul "Option" [prelude]
lst = modul "List" [opt, prelude]
test1 = modul "Test1" [test2]
test2 = modul "Test2" []
queen = modul "Queen" [arr, lst, test1, test2]

fromRight (Right a) = a

modulesTests = testGroup "Module dependency tests"
  [ testCase "Linearize includes" $ linearizeIncludes queen @?= [prelude, test2, arr, opt, test1, lst, queen] ]

parseOK s expected = testCase s $ case parseToplevel (T.pack s) of
    Right e -> assertEqual "" expected e
    Left  e -> assertFailure $ (show expected) ++ " but got " ++ Megaparsec.parseErrorPretty e

parseError s expected = testCase ("Error on " ++ s) $ case parseToplevel (T.pack s) of
    Right e -> assertFailure $ expected ++ " but got " ++ show e
    Left  e -> assertEqual "" expected $ Megaparsec.parseErrorTextPretty e

parseOkMatch s f = testCase s $ case parseToplevel (T.pack s) of
    Right p -> X.catch (f p) $ printErr (show p)
    Left  e  -> assertFailure $ Megaparsec.parseErrorPretty e

printErr :: String -> SomeException -> IO ()
printErr got e =  case fromException e of
    Just (PatternMatchFail s) -> assertFailure (s ++ "got " ++ got)
    nothing -> return ()

defaultModule = Module (withMetaPos 1 1) "<stdin>"

parseTypeOK s t = testCase s (parseType (T.pack s) @?= Right t)

parserTests2 = testGroup "Parser tests" [
      testCase "empty" $ parseToplevel "" @?= Right []
    , parseOkMatch "module Test_1 . Test2 ; "
        (\ [Module _ (NS (Name "Test_1") (Name "Test2")) ] -> return ())
    , parseOkMatch "import Test_1.Test2 ; import Test3"
        (\ [Import _ (NS (Name "Test_1") (Name "Test2")), Import _ "Test3" ]  -> return ())
    , parseError "module Test module Another"
        "unexpected 'm'\nexpecting end of input or import statement\n"
    , parseError "import  Asdf. "
        "unexpected '.'\nexpecting end of input, import statement, or top-level declaration\n"
    , parseError "import ._$#@ "
        "unexpected '.'\nexpecting qualified identifier (like My.Qualified.Name or SomeName)\n"
    , parseOkMatch "data Void" (\ [Data _ "Void" _ _] -> return ())
    , parseOkMatch "data Bool= |True|False" (\ [Data _ "Bool" [] [DataConst "True" [], DataConst "False" []]] -> return ())
    , parseOkMatch "data User = U(n: String, f)" (\ [Data _ "User" [] [DataConst "U" _]] -> return ())
    , parseError "data lower" "unexpected 'l'\nexpecting uppercase identifier\n"
    , parseError "data UppER=lower" "unexpected 'l'\nexpecting '|' or uppercase identifier\n"
    , parseTypeOK "String" (TypeIdent "String")
    , parseTypeOK "[Int]" (TypeApply (TypeIdent "Array") [TypeIdent "Int"])
    , parseTypeOK "(a -> B) -> C" (TypeFunc (TypeFunc (TVar $ TV "a") (TypeIdent "B")) (TypeIdent "C"))
    ]

parserTests = testGroup "Parser tests"
  [ testCase "Parse true" $
      parseExpr "true" @?= Right (Literal emptyMeta (BoolLit True))
  , testCase "Empty String" $
        parseExpr "\"\"" @?= Right (Literal emptyMeta (StringLit ""))
  , testCase "Character Escaping" $
      parseExpr "\"String\n\"" @?= Right (Literal emptyMeta (StringLit "String\n"))
  , testCase "String Interpolation" $
      parseExpr "\"Hello\\t \\\\\\$${ test123 + 1 }\"" @?= Right (Apply emptyMeta (Ident emptyMeta (NS "Prelude" "concat")) [
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
      parseAndInferExpr "match true { true -> 1 false -> 2 }" @?= TypeInt
  ]

data Mode = Dyn | Stat | Both
data Config = Script { name :: String, compMode :: Mode, arguments :: [T.Text] }

examples = [
    Script "builtin.lasca" Both [],
    Script "Array.lasca" Both [],
    Script "ArrayBuffer.lasca" Both [],
    Script "String.lasca" Both [],
    Script "List.lasca" Both [],
    Script "binarytrees.lasca" Both ["10"],
    Script "Data.lasca" Both [],
    Script "dynamic.lasca" Dyn [],
    Script "Either.lasca" Both [],
    Script "factorial.lasca" Both ["15"],
    Script "hello.lasca" Both [],
    Script "lambda.lasca" Both [],
    Script "Map.lasca" Both [],
    Script "Option.lasca" Both [],
    Script "regex.lasca" Both [],
    Script "queen.lasca" Both [],
    Script "ski.lasca" Both [],
    Script "nbody.lasca" Both ["50000"],
    Script "nbody2.lasca" Both ["50000"],
    Script "nbody3.lasca" Both ["50000"]
  ]

prependPath path script = script { name = path </> (name script) }
withMode s m = s { compMode = m }

mkGoldenTests s@(Script path mode args) = do
    let testName = takeBaseName path
    let goldenPath = "src" </> "test" </> "golden" </> replaceExtension path ".golden"
    let example = prependPath "examples" s
    let base = prependPath "libs/base" s
    e <- doesFileExist ("examples" </> path)
    let script = if e then example else base
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
            run "lasca" (["-e", "-O2", "--mode", "static", "--verbose", path] ++ extraArgs)
            run "lasca" (["-e", "-O2", "--mode", "dynamic", path] ++ extraArgs)

compileTests = [
        testProgram "Compile hello.lasca" "lasca" ["-O2", "-o", "hello", "examples/hello.lasca"] Nothing
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
    p <- TIO.readFile "libs/base/Prelude.lasca"
    let preludeExprs = fromRight $ parseToplevel p
    file <- TIO.readFile fname
    case parseToplevel file of
        Left err -> error $ Megaparsec.parseErrorPretty err
        Right ex -> do
            let exprs = preludeExprs ++ ex
            typeEnv <- typeCheck (emptyCtx emptyLascaOpts) exprs
            print typeEnv
            True @?= True
