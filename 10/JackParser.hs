-- big ups to https://hendrix-cs.github.io/csci360/modules/11-IMP.html
-- https://www.nand2tetris.org/project10
-- https://hendrix-cs.github.io/csci322/projects/10a-tokenizer.html

type FxnName   = String
type ClassName = String

data FxnType = Function

type FunctionCall = Maybe (Either ClassName VarName) -> FunctionName -> [Expr] -> String 

data Expr where 
  EInt      ::                 Int -> Expr
  ECall     ::        FunctionCall -> Expr
  
data Stmt where 
  Do     :: FunctionCall -> Stmt
  Return :: Maybe Expr -> Stmt 

data Type where 
  TInt    :: Type 
  TClass  :: String -> Type 

type Function = FxnType -> Maybe Type -> FxnName -> VarDecl -> VarDecl -> [Stmt]
 
data Decl where
  Fxn    :: Fxn -> Either Type Void -> String -> [(Type, String)] -> []
  
type Class = ClassName -> [Decl]
type Prog  = [Class]

type Keyword = Class |
               Constructor | Function | Method |
               Field | Static | Var |
               Int | Char | Boolean | Void | 
               True | False | Null | This | 
               Let | Do | If | Else | While | Return
               
type Symbol = Char 
One of: {  } (  ) [  ]  
        .  ,  ;  
        +  *  /  
        &  |  ~  
        <  > 
        =  -


--  ghc --make -o parser Downloads\nand2tetris\nand2tetris\projects\10\JackAnalyzer.hs
-- ./parser "Downloads\nand2tetris\nand2tetris\projects\10\Square\SquareGame.jack"
-- Downloads\nand2tetris\nand2tetris\tools\TextComparer.bat "Downloads\nand2tetris\nand2tetris\projects\10\Square\SquareT.xml" "Downloads\nand2tetris\nand2tetris\projects\10\Square\SquareGame.TT.xml"

import System.IO (readFile, writeFile)
import System.FilePath
import System.Environment
import Data.List

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
      let outputFile = (takeWhile (/='.') inputFile)++".vm"
      let xml = jackToXML (wordsWStrs $ splitter $ lines content)
      writeFile outputFile $ ("<tokens>\n"++unlines xml++"</tokens>")
      putStrLn $ "File '" ++ inputFile ++ "' has been written to '" ++ outputFile ++ "'."

splitter :: [String] -> String
splitter [] = []
splitter ([]:w)          = '\n':splitter w
splitter ((' ':l):w)     = ' ':splitter (l:w)
splitter (('\t':l):w)    = '\t':splitter (l:w)
splitter (('\r':l):w)    = splitter (l:w)
splitter (('/':'/':_):w) = splitter w
splitter (('/':'*':l):w) = splitter $ (longComment (l:w))
splitter ((c:l):w)       = let s = splitter (l:w) in 
  if elem [c] symbols then ' ':c:' ':s else c:s
  
wordsWStrs :: String -> [String]
wordsWStrs "" = []
wordsWStrs ('"':s) = let (a,b) = (takeWhile (/='"') s, tail $ dropWhile (/='"') s) in ('"':a):wordsWStrs b
wordsWStrs ('\r':s) = wordsWStrs s
wordsWStrs (c:s) | elem c " \t\n" = []:wordsWStrs s
wordsWStrs (c:s) = let ws = wordsWStrs s in if length ws < 1 then [[c]] else (c:head ws):tail ws  

longComment :: [String] -> [String]
longComment [] = []
longComment ([]:ss) = longComment ss
longComment (('*':'/':s):ss) = s:ss
longComment ((c:s):ss) = longComment (s:ss)


symbols = [[x] | x <- "{}()[].,;+*/|~=-"]
keywords = ["class","constructor","function","method","field","static","var","int","char","boolean","void","true","false","null","this","let","do","if","else","while","return"]

-- input split by word, output split by line.
jackToXML :: [String] -> [String]
jackToXML [] = []
jackToXML ([]:s) = jackToXML s
jackToXML (('<':l):s)                     = "<symbol> &lt; </symbol>":jackToXML   (l:s)
jackToXML (('>':l):s)                     = "<symbol> &gt; </symbol>":jackToXML   (l:s)
jackToXML (('&':l):s)                     = "<symbol> &amp; </symbol>":jackToXML  (l:s)
jackToXML (('"':l):s)                     = ("<stringConstant> " ++l++ " </stringConstant>"):jackToXML s
jackToXML ((c:w):s) | elem [c] symbols    = ("<symbol> "        ++[c]++ " </symbol>"):jackToXML s
jackToXML (w:s)     | elem w keywords     = ("<keyword> "         ++ w ++ " </keyword>"):jackToXML s
jackToXML ((c:w):s) | elem c "1234567890" = ("<integerConstant> " ++ (c:w) ++ " </integerConstant>"):jackToXML s
jackToXML (w:s)                           = ("<identifier> "      ++ w ++ " </identifier>"):jackToXML s


