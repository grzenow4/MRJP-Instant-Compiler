module Main where

import Data.Text.Lazy.Builder
import Data.Text.Lazy.IO as T (writeFile)
import System.Environment (getArgs)
import System.FilePath
import System.Process (system)

import Instant.ParInstant

import JVMCompiler (compile)

run :: String -> String -> IO Builder
run file s = case pProgram (myLexer s) of
    Left e -> error $ "Parsing error\n" ++ e
    Right parsed -> compile parsed file

main :: IO ()
main = do
    args <- getArgs
    case args of
        fs -> mapM_ (\file -> do
            content <- readFile file
            output <- run (dropExtension $ takeBaseName file) content
            let outputFile = replaceExtension file ".j"
            let outputDir = takeDirectory file
            T.writeFile outputFile (toLazyText output)
            system $ "java -jar lib/jasmin.jar " ++ outputFile ++ " -d " ++ outputDir) fs
