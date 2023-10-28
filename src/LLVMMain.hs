module Main where

import Data.Text.Lazy.Builder
import Data.Text.Lazy.IO as T (writeFile)
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import System.Process (system)

import Instant.ParInstant

import LLVMCompiler (compile)

run :: String -> IO Builder
run s = case pProgram (myLexer s) of
    Left e -> error $ "Parsing error\n" ++ e
    Right parsed -> compile parsed

main :: IO ()
main = do
    args <- getArgs
    case args of
        fs -> mapM_ (\file -> do
            content <- readFile file
            output <- run content
            let outputFile = replaceExtension file ".ll"
            let outputFile2 = replaceExtension file ".bc"
            T.writeFile outputFile (toLazyText output)
            system $ "llvm-link -o " ++ outputFile2 ++ " " ++ outputFile) fs
