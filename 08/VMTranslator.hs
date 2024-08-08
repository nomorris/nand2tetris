import qualified Data.Map as Map
import System.IO (readFile, writeFile)
import System.FilePath
import Data.List hiding (permutations, insert)
import Data.Maybe
import Text.Read
import System.Directory
import System.Environment

(//) = div
(%)  = mod

-- USAGE:
--   cd Downloads\nand2tetris\nand2tetris\projects\07
--   ghc --make -o vmt VMTranslator.hs
--   ./vmt "StackArithmetic\SimpleAdd\SimpleAdd.vm"

--https://hackage.haskell.org/package/haskell-gi-0.26.8/docs/src/Data.GI.CodeGen.Util.html#splitOn


endsInVM i = (snd $ splitAt (length i - 3) i) == ".vm"


main :: IO ()
main = getArgs >>= \args -> 
  case args of
    [i] | length i > 3 && endsInVM i -> translate i
    [dir] -> (listDirectory dir) >>= \files 
          -> translateDir dir (filter endsInVM files)
    _     -> putStrLn "I don't want to do that one."


-- listDirectory dir >>= \ds -> mapM_ (translate dir (concat $ tail $ splitOn '\\' dir)) (map ((dir++"\\")++) (filter endsInVM ds))


translateDir :: String -> [String] -> IO ()
translateDir dir files = do
 contents <- mapM readFile $ map ((dir++"/")++) files
 let ts = [parse (lines c) 0 (take (length file - 3) file) "" | (c,file) <- zip contents files]
 let outputFile = dir++'/':(last $ splitOn '/' dir)++".asm"
 writeFile outputFile (initInst ++ (unlines (concat ts)))
 putStrLn $ "File has been written to '" ++ outputFile ++ "'."

-- Main function
translate :: String -> IO ()
translate s = do
  content <- readFile s
  let outputFile = replaceExtension s ".asm"
  print (last $ splitOn '/' s)
  let asm = parse (lines content) 0 "Main" ""
  writeFile outputFile $ initInst ++ (unlines asm) -- ++ "(END)\n@END\n0;JMP"
  putStrLn $ "File has been written to '" ++ outputFile ++ "'."



popTo  idx ls j fn f= "D=M":('@':idx):"D=D+A":"@13":"M=D":"@SP":"A=M-1":"D=M":"@13":"A=M":"M=D":"@SP":"M=M-1":(parse ls j fn f)
pushTo idx ls j fn f= "D=M":('@':idx):"A=D+A":                          "D=M":"@SP":"A=M":"M=D":"@SP":"M=M+1":(parse ls j fn f)
jumper ls j sz  fn f= "@SP":"AM=M-1" :"D=M":"A=A-1":"A=M":"D=A-D":("@TRUE"++show j):
                     ("D;J"++sz):"@SP":"A=M-1":"M=0":("@FALSE"++show j):"0;JMP":
                     ("(TRUE"++show j++")"):"@SP":"A=M-1":"M=-1":("(FALSE"++show j++")"):
                     parse ls (j+1) fn f
binOp = ["@SP","AM=M-1","D=M","A=A-1"]
branch cond ls j fn f = "A=M":"D=A-D":
  ("@TRUE"++show j):("D;J"++cond):"@SP":"A=M-1":"M=0":("@FALSE"++show j):"0;JMP":
  ("(TRUE"++show j++")"):"@SP":"A=M-1":"M=-1":("(FALSE"++show j++")"):
  parse ls (j+1) fn f


parse :: [String] -> Int -> String -> String -> [String]

parse [] _ _ _ = []
parse (('/':'/':_):ls)         j file func = parse ls     j file func
parse ([]:ls)                  j file func = parse ls     j file func
parse ((' ':l):ls)             j file func = parse (l:ls) j file func
parse (('\t':l):ls)            j file func = parse (l:ls) j file func
parse (('\r':l):ls)            j file func = parse (l:ls) j file func



parse (('p':'o':'p':' ':l):ls) j file func = let seg:idx:_ = words l in case seg of 
  "constant" -> "@SP":"M=M-1":"D=M":('@':idx):"M=D":(parse ls j file func) 
  "local"    -> "@LCL":         popTo idx ls j file func 
  "static"   -> ('@':file++'.':idx):"D=A":"@15":"M=D":"@SP":"A=M-1":"D=M":"@15":"A=M":"M=D":"@SP":"M=M-1":(parse ls j file func)
  "this"     -> "@THIS":        popTo idx ls j file func 
  "that"     -> "@THAT":        popTo idx ls j file func 
  "temp"     -> "@5":"D=A":('@':idx):"D=D+A":"@15":"M=D":"@SP":"A=M-1":"D=M":"@15":"A=M":"M=D":"@SP":"M=M-1":(parse ls j file func)
  "argument" -> "@ARG":         popTo idx ls j file func 
  "pointer"  -> "@SP":"AM=M-1":"D=M":(if idx == "1" then "@THAT" else "@THIS"):"M=D":(parse ls j file func) 




parse (('p':'u':'s':'h':l):ls) j file func = let seg:idx:_ = words l in case seg of 
  "constant" -> ('@':idx):"D=A":"@SP":"A=M":"M=D":"@SP":"M=M+1":(parse ls j file func)
  "local"    -> "@LCL": pushTo idx ls j file func
  "static"   -> ('@':file++'.':idx):"D=M":"@SP":"A=M":"M=D":"@SP":"M=M+1":(parse ls j file func)
  "this"     -> "@THIS":pushTo idx ls j file func
  "that"     -> "@THAT":pushTo idx ls j file func
  "temp"     -> "@5":"D=A":('@':idx):"A=D+A":"D=M":"@SP":"A=M":"M=D":"@SP":"M=M+1":(parse ls j file func)
  "argument" -> "@ARG": pushTo idx ls j file func
  "pointer"  -> (if idx == "1" then "@THAT" else "@THIS"):"D=M":"@SP":"A=M":"M=D":"@SP":"M=M+1":(parse ls j file func)

parse (('a':'d':'d':_):ls)     j file func = binOp++"M=D+M":parse ls j file func
parse (('s':'u':'b':_):ls)     j file func = binOp++"M=M-D":parse ls j file func
parse (('a':'n':'d':_):ls)     j file func = binOp++"M=D&M":parse ls j file func
parse (('o':'r':_):ls)         j file func = binOp++"M=D|M":parse ls j file func

parse (('n':'e':'g':_):ls)     j file func = "@SP":"A=M-1" :"M=!M":"M=M+1":parse ls j file func
parse (('n':'o':'t':_):ls)     j file func = "@SP":"A=M-1" :"M=!M":parse ls j file func

parse (('l':'t':_):ls)         j file func = binOp++branch "LT" ls j file func
parse (('g':'t':_):ls)         j file func = binOp++branch "GT" ls j file func
parse (('e':'q':_):ls)         j file func = binOp++branch "EQ" ls j file func


parse (('l':'a':'b':'e':'l':l):ls) j file f = let symb:_ = words l in
  ('(':f++'$':symb++")"):parse ls j file f
parse (('g':'o':'t':'o':l):ls) j file func = let addr:_ = words l in 
  ('@':func++'$':addr):"0;JMP":parse ls j file func
parse (('i':'f':_:'g':'o':'t':'o':l):ls) j file func = let addr:_ = words l in 
  "@SP":"AM=M-1":"D=M":('@':func++"$"++addr):"D;JNE":parse ls j file func
  
parse (('f':'u':'n':'c':'t':'i':'o':'n':l):ls) j file func = let name:m:_ = words l in 
  ('(':name++")"):
  (concat $ replicate (read m) ["@SP","A=M","M=0","@SP","M=M+1"])++ 
  parse ls j file name
  
parse (('c':'a':'l':'l':l):ls) j fn f = let name:m:_ = words l in
  ('@':f++"$end"++show j):"D=A":"@SP":"A=M":"M=D":"@SP":"M=M+1":
  "@LCL" :"D=M":"@SP":"A=M":"M=D":"@SP":"M=M+1":
  "@ARG" :"D=M":"@SP":"A=M":"M=D":"@SP":"M=M+1":
  "@THIS":"D=M":"@SP":"A=M":"M=D":"@SP":"M=M+1":
  "@THAT":"D=M":"@SP":"A=M":"M=D":"@SP":"M=M+1":
  "@SP":"D=M":"@LCL":"M=D":
  "@5":"D=D-A":("@"++m):"D=D-A":"@ARG":"M=D":
  ('@':name):"0;JMP":
  ('(':f++"$end"++show j++")"):
  parse ls (j+1) fn f
  
parse (('r':'e':'t':'u':'r':'n':_):ls) j file func = 
  "@LCL":"D=M":"@15":"M=D":
  "@5":"A=D-A":"D=M":"@14":"M=D":
  "@SP":"AM=M-1":"D=M":"@ARG":"A=M":"M=D":
  "@ARG":"D=M+1":"@SP":"M=D":
  "@15":"AM=M-1":"D=M":"@THAT":"M=D":
  "@15":"AM=M-1":"D=M":"@THIS":"M=D":
  "@15":"AM=M-1":"D=M":"@ARG":"M=D":
  "@15":"AM=M-1":"D=M":"@LCL":"M=D":
  "@14":"A=M":"0;JMP":
  parse ls j file func
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
weave :: Char -> [String] -> String
weave _ [] = ""
weave _ [[c]] = [c]
weave c (s:ss) = s++c:weave c ss
  
-- call f n 
  -- push return-address
  -- push LCL
  -- push ARG
  -- push THIS
  -- push THAT
  -- ARG = SP-n-5
  -- LCL = SP
  -- goto f
  -- (return-address)
  
-- function f k :=
  -- (f)
  -- repeat k times:
    -- push 0
    
-- return :=
  -- FRAME = LCL
  -- RET = *(FRAME-5)
  -- *ARG = pop()
  -- SP = ARG+1
  -- THAT = *(FRAME-1)
  -- THIS = *(FRAME-2)
  -- ARG = *(FRAME-3)
  -- LCL = *(FRAME-4)
  -- goto RET
  
pushMA = ["D=M","@SP","A=M","M=D","@SP","M=M+1"]


initInst :: String
initInst =  "@256\nD=A\n@SP\nM=D\n" ++ unlines (parse ["call Sys.init 0"] (-1) "Sys" "Sys.o") 





splitOn :: Char -> String -> [String]
splitOn c [] = []
splitOn c s = go s [] 
  where go [] z     = [reverse z]
        go (y:ys) z = if c==y then reverse z : go ys [] else go ys (y:z)

permutations :: [a] -> [[a]]
permutations [] = []
permutations [x] = [[x]]
permutations (x:xs) = concat [[insert x i p | p <- permutations xs] | i <- [0..length xs]]

insert :: a -> Int -> [a] -> [a]
insert e 0 xs     = e:xs
insert e i (x:xs) = x:insert e (i-1) xs
