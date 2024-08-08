import System.IO (readFile, writeFile)
import System.FilePath
import System.Environment
import Data.List
import qualified Data.Map as Map

--  ghc --make -o parser Downloads\nand2tetris\nand2tetris\projects\10\JackCompiler3.hs
-- ./parser "Downloads\nand2tetris\nand2tetris\projects\10\Average\Main.jack"
-- Downloads\nand2tetris\nand2tetris\tools\TextComparer.bat "Downloads\nand2tetris\nand2tetris\projects\10\Square\SquareT.xml" "Downloads\nand2tetris\nand2tetris\projects\10\Square\SquareGame.TT.xml"

data Kind =  Static | Field | Arg | Var
type Table = Map.Map String (String, Kind, Int)

data XMLNode where 
  C :: Char   -> XMLNode
  K :: String -> XMLNode
  N :: Int    -> XMLNode
  S :: String -> XMLNode
  V :: String -> XMLNode  
  deriving (Show, Eq)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> translate inputFile
    _ -> putStrLn "I don't want to do that one."

(!) :: String -> (Table, Table) -> Maybe (String, Kind, Int)
s ! (gt,lt) = case (Map.lookup s lt, Map.lookup s gt) of 
  (Just t, Nothing) -> Just t
  (Nothing, Just t) -> Just t
  _ -> Nothing


translate :: String -> IO ()
translate s = do
  case s of
    inputFile -> do
      content <- readFile inputFile
      let outputFile = (takeWhile (/='.') inputFile)++".vm"
      let xml = jackToXML $ wordsWStrs $ splitter $ lines content
      let vm = parseClass xml "" "" Map.empty Map.empty
      writeFile outputFile (unlines vm) -- (unlines (map show xml)) 
      putStrLn $ "File '" ++ inputFile ++ 
                 "' has been written to '" ++ 
                 outputFile ++ "'."

parseClass :: [XMLNode] -> String -> String -> Table -> Table -> [String]
parseClass (K "class" : V v : C '{' : xs) _ fn _ lt = let (gt, xs', i) = parseFields xs Map.empty 0 in parseFxnDecs xs v fn gt lt i


parseFields :: [XMLNode] -> Table -> Int -> (Table, [XMLNode], Int)
parseFields (K "field" : typ : V id : C ';' : xs) gt i = parseFields xs (Map.insert id (parseType typ, Field, i) gt) (i+1)
parseFields (K "field" : typ : V id : C ',' : xs) gt i = parseFields (K "field" : typ : xs) (Map.insert id (parseType typ, Field, i) gt) (i+1)
parseFields (K "static" : typ : V id : C ';' : xs) gt i = parseFields xs (Map.insert id (parseType typ, Static, 0) gt) i
parseFields xs gt i = (gt, xs, i)


parseFxnDecs :: [XMLNode] -> String -> String -> Table -> Table -> Int -> [String]
parseFxnDecs (K fty : typ : V id : C '(' : xs) cn fn gt lt flds = 
  let (s, i, xs', t)     = parseParamList xs gt lt (if fty == "method" then 1 else 0) in 
  let (s', i', xs'', t') = parseFxnVarDec xs' t 0 in 
  let (s'', xs''')       = parseStmts xs'' gt t' cn in case fty of
    "function"    -> ("function "++cn++'.':id++' ':(show i')):s++s'++s''++parseFxnDecs xs''' cn "" gt lt flds
    "constructor" -> ("function "++cn++'.':id++' ':(show$i')):
                     ("push constant "++show (flds)):
                     "call Memory.alloc 1":"pop pointer 0":s++s'++s''++parseFxnDecs xs''' cn "" gt lt flds
    "method"      -> ("function "++cn++'.':id++' ':(show i')):"push argument 0":"pop pointer 0":s++s'++s''++parseFxnDecs xs''' cn "" gt lt flds
parseFxnDecs [] _ _ _ _ _ = []
parseFxnDecs (x:xs) a b c d e = parseFxnDecs xs a b c d e


parseParamList :: [XMLNode] -> Table -> Table -> Int -> ([String], Int, [XMLNode], Table) 
parseParamList (C ')' : C '{' : xs) gt lt i = ([], i, xs, lt)  
parseParamList (typ : V v : C ',' : xs) gt lt i = parseParamList xs gt (Map.insert v (parseType typ, Arg, i) lt) (i+1) 
parseParamList (typ : V v : C ')' : C '{' : xs) gt lt i = ([], i+1, xs, Map.insert v (parseType typ, Arg, i) lt)  
  
parseType :: XMLNode -> String 
parseType (K k) = k
parseType (V v) = v
  
parseFxnVarDec :: [XMLNode] -> Table -> Int -> ([String], Int, [XMLNode], Table) 
parseFxnVarDec (K "var" : typ : V v : C ',' : xs) t i = parseFxnVarDec (K "var" : typ : xs) (Map.insert v (parseType typ, Var, i) t) (i+1)
parseFxnVarDec (K "var" : typ : V v : C ';' : xs) t i = parseFxnVarDec xs (Map.insert v (parseType typ, Var, i) t) (i+1)
parseFxnVarDec xs t i = ([], i, xs, t)


parseStmts :: [XMLNode] -> Table -> Table -> String -> ([String], [XMLNode])
parseStmts xs@(K "if" : _)     gt lt c = let (s,x) = parseIf     xs gt lt c in let (s', x') = parseStmts x gt lt c in (s++s',x')
parseStmts xs@(K "do" : _)     gt lt c = let (s,x) = parseDo     xs gt lt c in let (s', x') = parseStmts x gt lt c in (s++s',x')
parseStmts xs@(K "while" : _)  gt lt c = let (s,x) = parseWhile  xs gt lt c in let (s', x') = parseStmts x gt lt c in (s++s',x')
parseStmts xs@(K "let" : _)    gt lt c = let (s,x) = parseLet    xs gt lt c in let (s', x') = parseStmts x gt lt c in (s++s',x')
parseStmts xs@(K "return" : _) gt lt c = let (s,x) = parseReturn xs gt lt c in let (s', x') = parseStmts x gt lt c in (s++s',x')
parseStmts (K "function" :xs)  gt lt c = ([],K "function" :xs)
parseStmts (C '}' :xs)         gt lt c = ([],xs)
parseStmts (x :xs)             gt lt c = parseStmts xs gt lt c
parseStmts [] _ _ _ = ([],[])
  

  
parseWhile :: [XMLNode] -> Table -> Table -> String -> ([String], [XMLNode])
parseWhile (K "while"  : xs) gt lt c = 
  let (bool, rest) = parseExpr xs gt lt c in 
  let (stmts, rest') = parseStmts (drop 1 rest) gt lt c in
  (["label WHILEBOOL"++show(length xs)]++
   bool++
   ["not", "if-goto WHILEEND"++show(length xs)]++
   stmts++
   ["goto WHILEBOOL"++show(length xs)]++
   ["label WHILEEND"++show(length xs)], rest')

parseLet :: [XMLNode] -> Table -> Table -> String -> ([String], [XMLNode])
parseLet (K "let" : V lst : C '[' : xs) gt lt c =
  case lst ! (gt,lt) of 
  Nothing -> ([],[])
  Just (ty, kn, i) ->    
    let (expr, C ']' : C '=' : rest) = parseExpr xs gt lt c in
    let (expr', C ';' : rest') = parseExpr rest gt lt c in 
    (("push "++parseKind kn++' ':show i):expr++
     ["add"]++expr'++
     ["pop temp 0", "pop pointer 1", "push temp 0", "pop that 0"], rest')
parseLet (K "let" : V v : C '=' : xs) gt lt c =
  let (s, x) = parseExpr xs gt lt c in 
    case v ! (gt,lt) of 
      Nothing -> ([],[])
      Just (ty, kn, i) ->    
        (s++[("pop " ++ parseKind kn ++ ' ':show i)], if x /= [] && head x == C ';' then tail x else x)

parseDo :: [XMLNode] -> Table -> Table -> String -> ([String], [XMLNode])
parseDo (K "do" : V cn : C '.' : V fn : C '(' : xs) gt lt classN = 
  let (exprs, i, rest) = parseExprList xs gt lt classN in 
  case cn ! (gt,lt) of 
    Nothing          -> (exprs++["call "++cn++"."++fn++" "++show i, "pop temp 0"], rest)  
    Just (ty,seg,ix) -> (("push "++parseKind seg++' ':show ix):exprs++["call "++ty++'.':fn++' ':show (i+1), "pop temp 0"], rest)
parseDo (K "do" : V fn : C '(' : xs) gt lt classN = 
  let (exprs, i, rest) = parseExprList xs gt lt classN in 
  ("push pointer 0":exprs++[("call "++classN++'.':fn++' ':show (i+1)), "pop temp 0"], rest)


parseIf :: [XMLNode] -> Table -> Table -> String -> ([String], [XMLNode])
parseIf (K "if" : xs) gt lt c = 
  let q = show $ length xs in 
  let (expr, C '{' : rest) = parseExpr xs gt lt c in 
  let (ifstmts, rest') = parseStmts rest gt lt c in 
  if rest' /= [] && head rest' == K "else" then
    let (elsestmts, rest'') = parseStmts (tail $ tail rest') gt lt c in 
    (expr++
    ("if-goto TRUE"++q):
    ("goto FALSE"++q):
    ("label TRUE"++q):
    ifstmts++
    ("goto IF"++q):
    ("label FALSE"++q):
    elsestmts++
    ["label IF"++q],
    rest'')
  else (expr++("if-goto TRUE"++q):("goto FALSE"++q):("label TRUE"++q):ifstmts++["label FALSE"++q], rest')
parseIf xs _ _ _ = ([], xs)

parseReturn :: [XMLNode] -> Table -> Table -> String -> ([String], [XMLNode])
parseReturn (K "return" : C ';' : xs) _ _ _ = (["push constant 0", "return"], xs)
parseReturn (K "return" : xs) gt lt c = let (s, C ';' : x) = parseExpr xs gt lt c in (s++["return"], tail xs)
  
  


parseExpr :: [XMLNode] -> Table -> Table -> String -> ([String], [XMLNode])
parseExpr (C ')' : xs) gt lt c = ([], xs) 
parseExpr xs gt lt c = let (s, xs') = parseTerm xs gt lt c in if xs' == [] then ([],xs) else case head xs' of 
  C o | elem o "+-/*<>=|&" -> let (s', xs'') = parseTerm (tail xs') gt lt c in (s++s'++[opToVM (C o)], xs'')
  _ -> (s, xs')
  

parseExprList :: [XMLNode] -> Table -> Table -> String -> ([String], Int, [XMLNode])
parseExprList [] _ _ _ = ([], 0, [])
parseExprList (C ')' : xs) _ _ _ = ([], 0, xs)
parseExprList a@(x : xs) gt lt c = 
  let (expr, rest) = parseExpr (if x==C ',' then xs else a) gt lt c in 
  let (s, i, x)    = parseExprList rest gt lt c in 
  (expr++s, i+1, x)

parseTerm :: [XMLNode] -> Table -> Table -> String -> ([String], [XMLNode])
parseTerm [] _ _ classN = ([], [])
parseTerm (V lst : C '[' : xs) gt lt classN = 
  case lst ! (gt,lt) of 
  Nothing -> ([],[])
  Just (ty, kn, i) ->    
    let (expr, C ']' : rest) = parseExpr xs gt lt classN in 
    (("push "++parseKind kn++' ':show i):expr++
     ["add", "pop pointer 1"]++
     ["push that 0"], rest)  
parseTerm (N n : xs) gt lt classN = (["push constant "++show n], xs)
parseTerm (S s : xs) gt lt classN = 
  let ss = parseString s charToInt in (("push constant "++show (length s)):"call String.new 1":ss, xs)
parseTerm (K "true":xs)  gt lt classN =  (["push constant 0", "not"], xs)
parseTerm (K "false":xs) gt lt classN = (["push constant 0" ], xs)
parseTerm (K "null":xs)  gt lt classN = (["push constant 0" ], xs)
parseTerm (K "this":xs)  gt lt classN = (["push pointer 0" ], xs)
parseTerm (C '(' : C '-' : xs) gt lt classN = let (s, C ')' : xs') = parseExpr xs gt lt classN in (s++["neg"], xs')
parseTerm (C '(' : C '~' : xs) gt lt classN = let (s, C ')' : xs') = parseExpr xs gt lt classN in (s++["not"], xs')
parseTerm (V cn : C '.' : V fn : C '(' : xs) gt lt classN = 
  let (exprs, i, rest) = parseExprList xs gt lt classN in 
  case cn ! (gt,lt) of 
    Nothing          -> (exprs++["call "++cn++"."++fn++" "++show i], rest)  
    Just (ty,seg,ix) -> (("push "++parseKind seg++' ':show ix):exprs++["call "++ty++'.':fn++' ':show (i+1)], rest)
parseTerm (V fn : C '(' : xs) gt lt classN = 
  let (exprs, i, rest) = parseExprList xs gt lt classN in 
  ("push pointer 0":exprs++["call "++classN++'.':fn++' ':show (i+1)], rest)
parseTerm (V id : xs) gt lt classN = 
  case id ! (gt,lt) of 
  Nothing -> ([],xs)
  Just (ty, kn, i) -> (["push "++(parseKind kn)++' ':show i], xs)
parseTerm (C '(' : xs) gt lt classN = let (s,x) = parseExpr xs gt lt classN in if x==[] then (s,[]) else (s,tail x)
parseTerm (C '-' : N n : xs) gt lt classN = (["push constant"++' ':show n, "neg"], xs)
parseTerm (_:xs) _ _ _ = ([],xs)


shunt :: [XMLNode] -> [XMLNode] -> [String]
shunt os [] = map opToVM os 
shunt os (C ')':xs) = 
  let (ps,qs) = splitAbout (C '(') os 
  in (map opToVM ps) ++ shunt (if qs==[] then [] else tail qs) xs 
shunt os ((C o):xs) | elem o "+-*/&|<>=(" = shunt (C o:os) xs 
shunt os (c:xs) = vmConst c : shunt os xs

opToVM :: XMLNode -> String
opToVM (C o) = case o of
  '+'->"add"
  '-'->"sub"
  '*'->"call Math.multiply 2"
  '/'->"call Math.divide 2"
  '&'->"and"
  '|'->"or"
  '<'->"lt"
  '>'->"gt"
  '='->"eq"
  _  -> ""
opToVM _ = ""

vmConst :: XMLNode -> String
vmConst (K "true")  = "push constant -1"
vmConst (K "false") = "push constant 0"
vmConst (N i)       = "push constant "++ show i
vmConst _ = "uh oh"


parseKind :: Kind -> String
parseKind Static = "static"
parseKind Field  = "this"
parseKind Arg    = "argument"
parseKind Var    = "local"

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c [] = []
splitOn c s = go s [] 
  where go [] z     = [reverse z]
        go (y:ys) z = if c==y then reverse z : go ys [] else go ys (y:z)

splitAbout :: Eq a => a -> [a] -> ([a], [a])
splitAbout x xs = (takeWhile (/=x) xs, dropWhile (/=x) xs)


charToInt :: Map.Map Char Int
charToInt = Map.fromList (zip (" !"++['"']++"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[/]^_`abcdefghijklmnopqrstuvwxyz{|}~") [32..126])

parseString :: String -> Map.Map Char Int -> [String]
parseString "" _      = []
parseString (c:s) cti = ("push constant " ++ show (cti Map.! c)) : "call String.appendChar 2" : parseString s cti

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
jackToXML (('"':l):s)                     = S l     : jackToXML s
jackToXML ((c:w):s) | elem [c] symbols    = C c     : jackToXML s
jackToXML (w:s)     | elem w keywords     = K w     : jackToXML s
jackToXML ((c:w):s) | elem c "1234567890" = N (read$c:w) : jackToXML s
jackToXML (w:s)                           = V w     : jackToXML s