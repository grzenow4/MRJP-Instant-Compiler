module Main where

import System.Environment
import System.FilePath
import System.Process

import Instant.ParInstant

import JVMCompiler (compile)

run :: String -> String -> IO String
run filename s = case pProgram (myLexer s) of
    Left e -> return $ "Parsing error\n" ++ e
    Right parsed -> compile parsed filename

main :: IO ()
main = do
    args <- getArgs
    case args of
        fs -> mapM_ (\file -> do
            content <- readFile file
            output <- run (dropExtension file) content
            let outputFile = replaceExtension file ".j"
            let outputDir = takeDirectory file
            writeFile outputFile output
            system $ "java -jar lib/jasmin.jar " ++ outputFile ++ " -d " ++ outputDir) fs
