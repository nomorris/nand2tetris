-- can you write me a haskell program that reads in a file of type ".ASM", reads it in, then writes it back out to a file of type ".txt"

import System.Environment (getArgs)
import System.IO (readFile, writeFile)
import Data.List (isSuffixOf)

-- Check if a file has the extension ".ASM"
isASMFile :: FilePath -> Bool
isASMFile filePath = ".ASM" `isSuffixOf` map toUpper (takeExtension filePath)

-- Convert the file extension to lowercase
toLowerExtension :: FilePath -> FilePath
toLowerExtension filePath = if isASMFile filePath then replaceExtension filePath ".txt" else filePath

-- Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> do
      content <- readFile inputFile
      let outputFile = toLowerExtension inputFile
      writeFile outputFile content
      putStrLn $ "File '" ++ inputFile ++ "' has been written to '" ++ outputFile ++ "'."
    _ -> putStrLn "Usage: program <inputFile.ASM>"
