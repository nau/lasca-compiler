module Main where

import System.IO

data Code = Code { codelines::String, bytes::Int }

genTopLevel idx = Code c bytes
    where
        c = "def test" ++ (show idx) ++ "() = 1;"
        bytes = length c

main :: IO ()
main = loop (Code "" 0) 0 0
    where
        loop code lastPrintedBytes idx = do
            let (Code newlines newbytes) = genTopLevel idx
            let updatedBytes = bytes code + newbytes
            let updatedCode = Code (codelines code ++ newlines) updatedBytes
            putStrLn newlines
            if (updatedBytes - lastPrintedBytes) > 1000000
            then do
                let kb = updatedBytes `div` 1000000
                hPutStrLn stderr ("Generated " ++ (show kb) ++ " Mb")
                loop updatedCode updatedBytes (idx + 1)
            else loop updatedCode lastPrintedBytes (idx + 1)




