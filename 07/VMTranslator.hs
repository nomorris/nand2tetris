import qualified Data.Map as Map
import System.IO (readFile, writeFile)
import System.FilePath
import System.Environment
import Data.List
import Data.Maybe
import Text.Read

(//) = div
(%)  = mod

-- USAGE:
--   cd Downloads\nand2tetris\nand2tetris\projects\07
--   ghc --make -o vmt VMTranslator.hs
--   ./vmt "StackArithmetic\SimpleAdd\SimpleAdd.vm"


main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> translate inputFile
    _ -> putStrLn "I don't want to do that one."

-- Main function
translate :: String -> IO ()
translate s = do
  case s of
    inputFile -> do
      content <- readFile inputFile
      let outputFile = replaceExtension inputFile ".asm"
      let insts = parse (lines content) 0
      writeFile outputFile $ initInst ++ (unlines insts) ++ "(END)\n@END\n0;JMP"
      putStrLn $ "File '" ++ inputFile ++ "' has been written to '" ++ outputFile ++ "'."


permutations :: [a] -> [a]
permutations [] = []
permutations (x:xs) = let ps = permutations xs in concat [[insert x i p | p <- ps] | i <- [0..length xs - 1]]

insert :: a -> Int -> [a] -> [a]
insert e 0 xs     = e:xs
insert e i (x:xs) = x:insert e (i-1) xs

-- Main function
translate :: String -> IO ()
translate s = do
  case s of
    inputFile -> do
      content <- readFile inputFile
      let outputFile = replaceExtension inputFile ".asm"
      let insts = parse (lines content) 0
      writeFile outputFile $ initInst ++ (unlines insts) ++ "(END)\n@END\n0;JMP"
      putStrLn $ "File '" ++ inputFile ++ "' has been written to '" ++ outputFile ++ "'."

pushDReg = ["@SP","A=M","M=D","@SP","M=M+1"]      
      
parse :: [String] -> Int ->  [String]
parse [] _ = []
parse (('/':'/':_):ls)         j = parse ls     j
parse ([]:ls)                  j = parse ls     j
parse ((' ':l):ls)             j = parse (l:ls) j
parse (('\t':l):ls)            j = parse (l:ls) j
parse (('\r':l):ls)            j = parse (l:ls) j
parse (('p':'o':'p':' ':l):ls) j = let seg:idx:_ = words l in case seg of 
  "constant" -> "@SP":"M=M-1":"D=M":('@':idx):"M=D":(parse ls j) 
  "local"    -> "@LCL":"D=M":('@':idx):"D=A+D":"@13":"M=D":"@SP":"A=M-1":"D=M":"@13":"A=M":"M=D":"@SP":"M=M-1":(parse ls j) 
  "static"   -> "@16":"D=M":('@':idx):"D=A+D":"@13":"M=D":"@SP":"A=M-1":"D=M":"@13":"A=M":"M=D":"@SP":"M=M-1":(parse ls j) 
  "this"     -> "@THIS":"D=M":('@':idx):"D=A+D":"@13":"M=D":"@SP":"A=M-1":"D=M":"@13":"A=M":"M=D":"@SP":"M=M-1":(parse ls j) 
  "that"     -> "@THAT":"D=M":('@':idx):"D=A+D":"@13":"M=D":"@SP":"A=M-1":"D=M":"@13":"A=M":"M=D":"@SP":"M=M-1":(parse ls j) 
  "temp"     -> "@5":"D=A":('@':idx):"D=A+D":"@13":"M=D":"@SP":"A=M-1":"D=M":"@13":"A=M":"M=D":"@SP":"M=M-1":(parse ls j) 
  "argument" -> "@ARG":"D=M":('@':idx):"D=A+D":"@13":"M=D":"@SP":"A=M-1":"D=M":"@13":"A=M":"M=D":"@SP":"M=M-1":(parse ls j) 
  "pointer"  -> "@SP":"AM=M-1":"D=M":(if idx == "1" then "@THAT" else "@THIS"):"M=D":(parse ls j) 
parse (('p':'u':'s':'h':l):ls) j = let seg:idx:_ = words l in case seg of 
  "constant" ->              ('@':idx):"D=A":        "@SP":"A=M":"M=D":"@SP":"M=M+1":(parse ls j)
  "local"    -> "@LCL":"D=M":('@':idx):"A=D+A":"D=M":"@SP":"A=M":"M=D":"@SP":"M=M+1":(parse ls j)
  "static"   -> "@16":"D=M":('@':idx):"A=D+A":"D=M":"@SP":"A=M":"M=D":"@SP":"M=M+1":(parse ls j)
  "this"     -> "@THIS":"D=M":('@':idx):"A=D+A":"D=M":"@SP":"A=M":"M=D":"@SP":"M=M+1":(parse ls j)
  "that"     -> "@THAT":"D=M":('@':idx):"A=D+A":"D=M":"@SP":"A=M":"M=D":"@SP":"M=M+1":(parse ls j)
  "temp"     -> "@5":"D=A":('@':idx):"A=D+A":"D=M":"@SP":"A=M":"M=D":"@SP":"M=M+1":(parse ls j)
  "argument" -> "@ARG":"D=M":('@':idx):"A=D+A":"D=M":"@SP":"A=M":"M=D":"@SP":"M=M+1":(parse ls j)
  "pointer"  -> (if idx == "1" then "@THAT" else "@THIS"):"D=M":"@SP":"A=M":"M=D":"@SP":"M=M+1":(parse ls j)
parse (('a':'d':'d':_):ls)     j = "@SP":"AM=M-1":"D=M":"A=A-1":"M=D+M":parse ls j
parse (('s':'u':'b':_):ls)     j = "@SP":"AM=M-1":"D=M":"A=A-1":"M=M-D":parse ls j
parse (('a':'n':'d':_):ls)     j = "@SP":"AM=M-1":"D=M":"A=A-1":"M=D&M":parse ls j
parse (('o':'r':_):ls)         j = "@SP":"AM=M-1":"D=M":"A=A-1":"M=D|M":parse ls j
parse (('n':'e':'g':_):ls)     j = "@SP":"A=M-1" :"M=!M":"M=M+1":parse ls j
parse (('n':'o':'t':_):ls)     j = "@SP":"A=M-1" :"M=!M":parse ls j
parse (('l':'t':_):ls)         j = "@SP":"AM=M-1" :"D=M":"A=A-1":"A=M":"D=A-D":("@TRUE"++show j):"D;JLT":"@SP":"A=M-1":"M=0":("@FALSE"++show j):"0;JMP":("(TRUE"++show j++")"):"@SP":"A=M-1":"M=-1": ("(FALSE"++show j++")"):parse ls (j+1)
parse (('g':'t':_):ls)         j = "@SP":"AM=M-1" :"D=M":"A=A-1":"A=M":"D=A-D":("@TRUE"++show j):"D;JGT":"@SP":"A=M-1":"M=0":("@FALSE"++show j):"0;JMP":("(TRUE"++show j++")"):"@SP":"A=M-1":"M=-1": ("(FALSE"++show j++")"):parse ls (j+1)
parse (('e':'q':_):ls)         j = "@SP":"AM=M-1" :"D=M":"A=A-1":"A=M":"D=A-D":("@TRUE"++show j):"D;JEQ":"@SP":"A=M-1":"M=0":("@FALSE"++show j):"0;JMP":("(TRUE"++show j++")"):"@SP":"A=M-1":"M=-1": ("(FALSE"++show j++")"):parse ls (j+1)





initInst :: String
initInst = "@256\nD=A\n@SP\nM=D\n"