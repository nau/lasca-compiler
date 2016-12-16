-- Simplest program ever
extern println(arg);

def log() = console.log 1 2 3;

def main(arg) = println arg;

{-cgen :: S.Expr -> Codegen AST.Operand
cgen (S.IntLit n) = return (cons (C.Int 32 n))
cgen app@(S.App _ _) = do
  let (S.Call fn args) = S.flattenApp app
  largs <- mapM cgen args
  call (externi32 (AST.Name fn)) largs
  -}