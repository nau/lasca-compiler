module Main where

import Control.Monad
import Control.Monad.Trans
import Data.Char
import System.Console.Haskeline
import System.Process
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Prim
import Text.Parsec.String
import Text.Parsec.Language
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Text.Parsec.Token as Token
import Syntax



fisDef = Token.LanguageDef
     { Token.commentStart   = "#"
     , Token.commentEnd     = ""
     , Token.commentLine    = "#"
     , Token.nestedComments = False
     , Token.identStart     = letter <|> char '_'
     , Token.identLetter    = alphaNum <|> oneOf "_'"
     , Token.opStart        = Token.opLetter fisDef
     , Token.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
     , Token.reservedOpNames= ["+", "-", "*", "/", "<", ">", "|"]
     , Token.reservedNames  = ["let"]
     , Token.caseSensitive  = True
     }

lexer           = Token.makeTokenParser fisDef
lexeme           = Token.lexeme lexer
whiteSpace      = Token.whiteSpace lexer
stringLiteral   = Token.stringLiteral    lexer
integer         = Token.integer    lexer
parens          = Token.parens     lexer
brackets        = Token.brackets   lexer
braces        = Token.braces   lexer
reservedOp      = Token.reservedOp lexer
reserved        = Token.reserved   lexer
semi            = Token.semi   lexer
comma            = Token.comma   lexer
identifier      = Token.identifier lexer
symbol      = Token.symbol lexer
stringLetter    = (satisfy (\c -> (c > '\026') && not (c `elem` "\"\\=;<>|"))) -- (c /= '"') && (c /= '\\') && (c > '\026') && (c /= '='))

--whileParser :: Parser Stmt
whileParser = do
            whiteSpace
            s <- program
            eof
            return s

program = statements

statements = sequenceOfStmt

sequenceOfStmt =
    do list <- many1 statement
       return (Seq list)

statement = do
            s <- statement'
            semi
            return s

--statement' :: Parser Stmt
statement' =  try funDecl
        <|> try assignStmt
        <|> command <?> "command"

funDecl = do
    name <- identifier
    args <- parens (many identifier)
    body <- braces statements
    return $ Fun name args body

--assignStmt :: Parser Stmt
assignStmt =
   do
      reserved "let"
      var  <- identifier
      reservedOp "="
      expr <- assignExpr
      vars <- getState
      pos <- getPosition
      let assign = Assign var expr
      let update o _ = error ("constant '" ++ var ++ "' already defined '" ++ (translateToBash assign) ++ "' at " ++ (show o))
      modifyState (Map.insertWith' update var pos)
      return assign

assignExpr = listTerm <|> stringTerm <|> aExpression

stringTerm = liftM StrLit stringLiteral

--aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

aOperators =  [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
              , [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft,
                 Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft]
              , [Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft,
                 Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft]
              ]

aTerm =  parens aExpression
      <|> liftM Ident identifier
      <|> int

int = liftM IntConst integer

listTerm = brackets listDef

listDef = (listType aExpression) <|> (listType stringTerm)

listType e = do
        list <- sepBy1 e comma
        return $ NList list

--command :: Parser Stmt
command = do
          cmd <- identifier <?> "command name"
          as <- args <?> "args"
          return $ Cmd cmd as --red

--args :: Parser [CmdArg]
args = many cmdArgument

--cmdArgument :: Parser CmdArg
cmdArgument =  strArg <|> arg

--strArg :: Parser CmdArg
strArg = liftM StrArg stringLiteral

--arg :: Parser CmdArg
arg = lexeme $ try $
        do
        mm <- many1 (stringLetter)
        return (Arg mm)

--redir = string ">" >> arg

parseString :: String -> Stmt
parseString str =
   case runParser whileParser Map.empty "" str of
   Left e  -> error $ show e
   Right r -> r

execShellScript :: String -> IO ()
execShellScript = callCommand


translateToBash :: Stmt -> String
translateToBash (Seq []) = ""
translateToBash (Seq [s]) = translateToBash s
translateToBash (Seq (s:ss)) = left ++ ";\n" ++ right
    where
        left = translateToBash s
        right = translateToBash $ Seq ss
translateToBash (Fun name args body) = name ++ "() {\n" ++ genargs ++ (translateToBash body) ++ "\n}"
    where
        genargs = List.concat lines
        lines = map localargs (zip args [1..])

        localargs :: (String, Int) -> String
        localargs (name, i) = "local " ++ name ++ "=$" ++ (show i) ++ "\n"
translateToBash (Assign ident value) = ident ++ "=" ++ exprToBash value
translateToBash (Cmd cmd args) = cmd ++ " " ++ aaa -- ++ red
    where
        subst :: [CmdArg] -> [String]
        subst [] = []
        subst (a:aa) = (arg2str a) : subst aa
        aaa = unwords $ subst args

--        red = foldl join " " redir
--        join a (Redir s f) = a ++ s ++ f

arg2str :: CmdArg -> String
arg2str (Arg a) = a
arg2str (StrArg a) = show a

exprToBash :: AExpr -> String
exprToBash (Ident s) = s
exprToBash (IntConst i) = show i
exprToBash (StrLit i) = "\"" ++ i ++ "\""
exprToBash (NList []) = "()"
exprToBash (NList ls) = "(" ++ elems ++ ")"
    where
        elems = (List.intercalate "," (asdf ls))
        asdf ls = case ls of
            [] -> []
            (e:ee) -> (exprToBash e) : (asdf ee)
exprToBash (Neg expr) = "-(" ++ (exprToBash expr) ++ ")"
exprToBash (ABinary Add lhs rhs) = exprToBash lhs ++ "+" ++ exprToBash rhs
exprToBash (ABinary Subtract lhs rhs) = exprToBash lhs ++ "-" ++ exprToBash rhs
exprToBash (ABinary Multiply lhs rhs) = exprToBash lhs ++ "*" ++ exprToBash rhs
exprToBash (ABinary Divide lhs rhs) = exprToBash lhs ++ "/" ++ exprToBash rhs



main :: IO ()
main = do
  f <- (readFile "test.txt")
--  r <- parseFile f
--  print r
  let tree = parseString f
  print $ tree
  putStr $ translateToBash $ parseString f
  (execShellScript . translateToBash . parseString) f
