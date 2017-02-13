module Main where

import System.IO
import System.Exit
import System.Environment

genTopLevel idx = "def test" ++ (show idx) ++ "() = let x = 1+1 in if x > 2 then 1 else 2 end end"

genNLines :: Integer -> IO ()
genNLines n = loop "" 0 0
    where
      loop code lastPrintedLines idx = do
          let newline = genTopLevel idx
          let updatedCode = code ++ newline
          putStrLn newline
          if (idx) >= n
          then hPutStrLn stderr ("Generated " ++ (show idx) ++ " lines")
          else if (idx - lastPrintedLines) > 1000
               then do
                  let kloc = idx `div` 1000
                  hPutStrLn stderr ("Generated " ++ (show kloc) ++ " klocs")
                  loop updatedCode idx (idx + 1)
               else loop updatedCode lastPrintedLines (idx + 1)

main :: IO ()
main = do
    args <- getArgs

    putStrLn "def main() = 123 end"

    let numLines = case args of
            []      -> 1000
            [count] -> read count :: Integer
    genNLines numLines




