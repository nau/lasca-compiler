{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Main where

import Data.Char
import Text.Parsec hiding (State)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Prim
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Indent
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified LLVM.General.AST as AST

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline
-- import NeatInterpolation
import Syntax
import Emit
import Codegen
{-# ANN module ("HLint: ignore Eta reduce"::String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"::String) #-}


-- Lexer
reservedOps :: [String]
reservedOps = [
    "->",
    "\\",
    "+",
    "*",
    "-",
    "="
  ]

reservedNames :: [String]
reservedNames = [
   "def", "extern"
  ]

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
     { Tok.commentStart    = "{-"
     , Tok.commentEnd      = "-}"
     , Tok.commentLine     = "--"
     , Tok.nestedComments = False
     , Tok.identStart     = letter
     , Tok.identLetter    = alphaNum
     , Tok.opStart        = Tok.opLetter langDef
     , Tok.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
     , Tok.reservedOpNames= reservedOps
     , Tok.reservedNames  = reservedNames
     , Tok.caseSensitive  = True
     }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

integerLexem    = Tok.integer lexer
parens     = Tok.parens lexer
commaSep   = Tok.commaSep lexer
semiSep    = Tok.semiSep lexer
identifierLexem = Tok.identifier lexer
whitespace = Tok.whiteSpace lexer
reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer

-- Parser
integer :: Parser Integer
integer = do
        f <- Tok.lexeme lexer sign
        n <- decimal
        return (f n)
    where

        sign         =  (char '-' >> return negate)
                    <|> (char '+' >> return id)
                    <|> return id

        decimal         = number 10 digit
        number base baseDigit
                = do{ digits <- many1 baseDigit
                    ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
                    ; seq n (return n)
                    }

intLit :: Parser Expr
intLit = do
    i <- integer
    return (IntLit i)

intLitLexem :: Parser Expr
intLitLexem = do
    i <- integerLexem
    return (IntLit i)

identifier :: Parser String
identifier = do{ c <- Tok.identStart langDef
            ; cs <- many (Tok.identLetter langDef)
            ; return (c:cs)
            }
        <?> "identifier"

ident :: Parser Expr
ident = do i <- identifier; return (Ident i)

identLexem :: Parser Expr
identLexem = do i <- identifierLexem; return (Ident i)

aexp :: Parser Expr
aexp = parens expr
   <|> intLitLexem
   <|> identLexem

callExpr :: Parser Expr
callExpr = between (Tok.symbol lexer "(") (string ")") expr
    <|> ident
    <|> intLit

methodCall :: Parser Expr
methodCall =
  do
    e <- callExpr
    reservedOp "."
    func <- identLexem
    es <- many aexp
    return (foldl1 App (func : e : es))

apply :: Parser Expr
apply = do
  es <- many1 aexp
  return (foldl1 App es)

expr :: Parser Expr
expr = try methodCall
    <|> apply

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many identifier
  return $ Extern name args

function :: Parser Expr
function = do
    reserved "def"
    name <- identifier
    args <- parens $ commaSep identifier
    reservedOp "="
    body <- expr
    return $ Def name args body

defn :: Parser Expr
defn = try extern
    <|> try function
    <|> expr


contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

-- parseExpr :: String -> Either ParseError Expr
-- parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s



toJs :: Expr -> String
toJs (IntLit i) = show i
toJs (Ident n) = n
toJs (Extern fn args) = ""
toJs (Def name args body) = "function " ++ name ++ "(" ++ (List.intercalate ", " args) ++ ") { return " ++ (toJs body) ++ ";}"
toJs app@(App _ _) =
        obj ++ "." ++ fn ++ "(" ++ (List.intercalate ", " ars) ++ ")"
    where
        (fn : obj : ars) = args app []

        args :: Expr -> [String] -> [String]
        args (App l r) xs =  args l ((toJs r) : xs)
        args e xs =  toJs e : xs


initModule :: AST.Module
initModule = emptyModule "my cool jit"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex

      return $ Just ast

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
  loop mod = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        modn <- liftIO $ process mod input
        case modn of
          Just modn -> loop modn
          Nothing -> loop mod

readInput = loop
    where
    loop = do
        line <- getLine
        let res = parseToplevel line
        case res of
            Left err ->
                loop
            Right r -> do
                putStrLn $ show (length (show r))
                loop

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> readInput
    [fname] -> processFile fname >> return ()