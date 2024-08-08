-- big ups to https://hendrix-cs.github.io/csci360/modules/11-IMP.html
-- https://www.nand2tetris.org/project10
-- https://hendrix-cs.github.io/csci322/projects/10a-tokenizer.html

-- String Types
type FxnName   = String
type ListName  = String
type VarName   = String
type ClassName = String


data Keyword = KTrue | KFalse | KThis | KNull
data FxnType = Constructor | Function | Method
data UOp     = Neg | Not
data BOp     = Add | Sub | Mul | Div | And | Or | Equals | Less | More

type FunctionCall = Maybe (Either ClassName VarName) -> FunctionName -> [Expr] -> String

data Expr where 
  EInt      ::                 Int -> Expr
  EString   ::              String -> Expr
  EKey      ::             Keyword -> Expr
  EVar      ::             VarName -> Expr
  EListElem ::   ArrayName -> Expr -> Expr
  EParen    ::                Expr -> Expr
  EUOp      ::         UOp -> Expr -> Expr
  EBOp      :: BOp -> Expr -> Expr -> Expr
  ECall     ::        FunctionCall -> Expr
  
data Stmt where 
  Let    :: VarName -> Maybe Expr -> Expr -> Stmt 
  If     :: Expr -> [Stmt] -> (Maybe [Stmt]) -> Stmt 
  While  :: Expr -> [Stmt] -> Stmt 
  Do     :: FunctionCall -> Stmt
  Return :: Maybe Expr -> Stmt 

data Type where 
  TInt    :: Type 
  TBool   :: Type 
  TChar   :: Type 
  TClass  :: String -> Type 

type VarDecl = [(Type, [VarName])]

type Function = FxnType -> Maybe Type -> FxnName -> VarDecl -> VarDecl -> [Stmt]
 
data Decl where
  Static :: Type -> [VarName] -> Decl 
  Field  :: Type -> [VarName] -> Decl 
  Fxn    :: Fxn -> Either Type Void -> String -> [(Type, String)] -> []
  
type Class = ClassName -> [Decl]
type Prog = [Class]


data XMLNode where 
  Sym :: Char   -> XMLNode
  Key :: String -> XMLNode
  Num :: Int    -> XMLNode
  Str :: String -> XMLNode
  Idn :: String -> XMLNode  
  Var :: String -> XMLNode  
  Arg :: String -> XMLNode  
  Stt :: String -> XMLNode  
  Fld :: String -> XMLNode  
  Cls :: String -> XMLNode  
  Srt :: String -> XMLNode 
  deriving (Show, Eq)



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
      let xml = jackToXML $ wordsWStrs $ splitter $ lines content
      writeFile outputFile $ ("<tokens>\n"++unlines xml++"</tokens>")
      putStrLn $ "File '" ++ inputFile ++ "' has been written to '" ++ outputFile ++ "'."








-- THREE:
-- class Main {
   -- function void main() {
      -- do Output.printInt(3);
      -- return;
   -- }

-- SEVEN:
-- class Main {
   -- function void main() {
      -- do Output.printInt(1 + (2 * 3));
      -- return;
   -- }
-- }

data Expr where 
  EInt      ::                 Int -> Expr
  EString   ::              String -> Expr
  EKey      ::             Keyword -> Expr
  EVar      ::             VarName -> Expr
  EListElem ::   ArrayName -> Expr -> Expr
  EParen    ::                Expr -> Expr
  EUOp      ::         UOp -> Expr -> Expr
  EBOp      :: BOp -> Expr -> Expr -> Expr
  ECall     ::        FunctionCall -> Expr
  
data Stmt where 
  Let    :: VarName -> Maybe Expr -> Expr -> Stmt 
  If     :: Expr -> [Stmt] -> (Maybe [Stmt]) -> Stmt 
  While  :: Expr -> [Stmt] -> Stmt 
  Do     :: FunctionCall -> Stmt
  Return :: Maybe Expr -> Stmt 

data XMLNode where 
  Sym :: Char   -> XMLNode
  Key :: String -> XMLNode
  Num :: Int    -> XMLNode
  Str :: String -> XMLNode
  Idn :: String -> XMLNode  
  deriving (Show, Eq)

type FunctionCall = Maybe (Either ClassName VarName) -> FunctionName -> [Expr] -> String 


parseExpr :: [XMLNode] -> Expr
parseExpr []            = EInt 0
parseExpr [Num x]       = EInt x
parseExpr [Str s]       = EString s
parseExpr [Key  "true"] = EInt 1
parseExpr [Key "false"] = EInt 0
parseExpr [Key  "void"] = EInt 0
parseExpr [Key  "this"] = EInt 0
parseExpr (Idn v : Sym '[' : ts) = EListElem v (parseExpr $ init ts) 
-- parseExpr (Sym '(' : ts )

-- THREE:
-- class Main {
   -- function void main() {
      -- do Output.printInt(3);
      -- return;
   -- }

-- SEVEN:
-- class Main {
   -- function void main() {
      -- do Output.printInt(1 + (2 * 3));
      -- return;
   -- }
-- }

toJack :: [[XMLNode]] -> [String]
toJack [] = []
toJack (Key "class":ns) = 
toJack (Key "function":ns) = 
toJack (Key "void"    :ns) = 
toJack (Idn s         :ns) = 
toJack (Key "do"      :Idn cn : Sym '.' : Idn fn : Sym '(' : ns) = let (xs, ys) = splitOn (Sym ';') ns in 
toJack (Key "return") = 

exprToJack = [XMLNode] -> [String]
exprToJack (Num n:ns) = ("push "++show n):toJack ns 
exprToJack (Idn v:ns) = ("push "++show v):toJack ns 
exprToJack (Sym '+' : ns) = let (xs:ys) = splitAt (Sym ')') ns in 



-- toJack (Key "do"      :Idn fn : ns) = let (xs, ys) = splitOn (Sym ';') ns in 
   
toJack (Key "class"   :Idn cn : ns) = 
toJack 




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


symbols = [[x] | x <- "<>&{}()[].,;+*/|~=-"]
keywords = ["class","constructor","function","method","field","static","var","int","char","boolean","void","true","false","null","this","let","do","if","else","while","return"]


-- input split by word, output split by line.
jackToXML :: [String] -> [XMLNode]
jackToXML [] = []
jackToXML ([]:s) = jackToXML s
jackToXML (('"':l):s)                     = Str l     : jackToXML s
jackToXML ((c:w):s) | elem [c] symbols    = Sym c     : jackToXML s
jackToXML (w:s)     | elem w keywords     = Key w     : jackToXML s
jackToXML ((c:w):s) | elem c "1234567890" = Num (c:w) : jackToXML s
jackToXML (w:s)                           = Idn w     : jackToXML s


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c [] = []
splitOn c s = go s [] 
  where go [] z     = [reverse z]
        go (y:ys) z = if c==y then reverse z : go ys [] else go ys (y:z)
