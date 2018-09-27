{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
module TestTH where
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Golden as G
import Test.Tasty.Program
import System.FilePath
import System.FilePath.Glob
import System.Directory
import qualified Text.Megaparsec as Megaparsec
import Shelly (shelly, run)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Lasca.Parser
import Lasca.Syntax
import Lasca.Infer
import Lasca.Type
import Lasca.Options
import Lasca.Modules

import Data.List
import Data.Ord
import Data.Foldable

parseMatch :: String -> Q Pat -> Q Exp
parseMatch s p = [|testCase s $ case parseToplevel (T.pack s) of
    $p -> return ()
    e -> assertFailure $ " but got " ++ show e
  |]

parseOkMatch :: String -> Q Pat -> Q Exp
parseOkMatch s p = do
    pat <- runQ p
    let spat = pprint pat
    [| testCase s $ case parseToplevel (T.pack s) of
        Right $p -> return ()
        Left  e  -> assertFailure $ spat ++ " but got " ++ Megaparsec.parseErrorPretty e
        e -> assertFailure $ spat ++ " but got " ++ show e |]
