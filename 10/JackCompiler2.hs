import System.IO (readFile, writeFile)
import System.FilePath
import System.Environment
import Data.List
import qualified Data.Map as Map

--  ghc --make -o parser Downloads\nand2tetris\nand2tetris\projects\10\JackCompiler2.hs
-- ./parser "Downloads\nand2tetris\nand2tetris\projects\10\Factorial\Main.jack"
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

charToInt :: Map.Map Char Int
charToInt = Map.fromList (zip (" !"++['"']++"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[/]^_`abcdefghijklmnopqrstuvwxyz{|}~") [32..126])

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> translate inputFile
    _ -> putStrLn "I don't want to do that one."



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
parseClass (K "class" : V v : C '{' : xs) _ fn gt lt = parseFxnDecs xs v fn gt lt

parseFxnDecs :: [XMLNode] -> String -> String -> Table -> Table -> [String]
parseFxnDecs (K "function" : K typ : V id : C '(' : xs) cn fn gt lt = 
  let (s, i, xs', t)     = parseParamList xs lt 0 in 
  let (s', i', xs'', t') = parseFxnVarDec xs' t 0 in 
  let (s'', xs''')       = parseStmts xs'' t' in 
  let ss = parseFxnDecs xs''' cn "" gt t' in 
  ("function "++cn++'.':id++' ':(show i')):s++s'++s''++ss
parseFxnDecs (C '}' : xs) _ _ _ _ = []
parseFxnDecs [] _ _ _ _ = []

parseParamList :: [XMLNode] -> Table -> Int -> ([String], Int, [XMLNode], Table) 
parseParamList (C ')' : C '{' : xs) t i = ([], i, xs, t)  
parseParamList (K typ : V v : C ',' : xs) t i = 
  let (s,_,x,_) = parseParamList xs t i 
  in (s, i+1, x, Map.insert v (typ, Arg, i) t)  
parseParamList (K typ : V v : C ')' : C '{' : xs) t i = 
  ([], i+1, xs, Map.insert v (typ, Arg, i) t)  
  
parseFxnVarDec :: [XMLNode] -> Table -> Int -> ([String], Int, [XMLNode], Table) 
parseFxnVarDec (K "var" : K typ : V v : C ',' : xs) t i = 
  let (s,i',x,t') = parseFxnVarDec (K "var" : K typ : xs) (Map.insert v (typ, Var, i) t) (i+1)
  in (s, i', x, t')
parseFxnVarDec (K "var" : K typ : V v : C ';' : xs) t i = 
  let (s, i', x, t') = parseFxnVarDec xs (Map.insert v (typ, Var, i) t) (i+1)
  in (s, i', x, Map.insert v (typ, Var, i) t')  
parseFxnVarDec xs t i = ([], i, xs, t)


parseStmts :: [XMLNode] -> Table -> ([String], [XMLNode])
parseStmts xs@(K "if" : _) t     = let (s,x) = parseIf xs t in let (s', x')     = parseStmts x t in (s++s',x')
parseStmts xs@(K "do" : _) t     = let (s,x) = parseDo xs t     in let (s', x') = parseStmts x t in (s++s',x')
parseStmts xs@(K "while" : _) t  = let (s,x) = parseWhile xs t  in let (s', x') = parseStmts x t in (s++s',x')
parseStmts xs@(K "let" : _) t    = let (s,x) = parseLet xs t    in let (s', x') = parseStmts x t in (s++s',x')
parseStmts xs@(K "return" : _) t = let (s,x) = parseReturn xs t in let (s', x') = parseStmts x t in (s++s',x')
parseStmts (C '}' :xs) t         = ([],xs)
parseStmts (C ';' :xs) t         = parseStmts xs t
parseStmts _ _ = ([],[])
  
parseWhile :: [XMLNode] -> Table -> ([String], [XMLNode])
parseWhile (K "while"  : xs) gt = 
  let (bool, rest) = parseExpr xs gt in 
  let (stmts, rest') = parseStmts (drop 1 rest) gt in
  (["label WHILEBOOL"++show(length xs)]++
   bool++
   ["not", "if-goto WHILEEND"++show(length xs)]++
   stmts++
   ["goto WHILEBOOL"++show(length xs)]++
   ["label WHILEEND"++show(length xs)], rest')



parseDo :: [XMLNode] -> Table -> ([String], [XMLNode])
parseDo (K "do" : V cn : C '.' : V fn : C '(' : xs) t = 
  let (exprs, i, rest) = parseExprList xs t in 
  (exprs++["call "++cn++"."++fn++" "++show i, "pop temp 0"], rest)  

parseIf :: [XMLNode] -> Table -> ([String], [XMLNode])
parseIf (K "if" : xs) t = 
  let q = show $ length xs in 
  let (expr, C '{' : rest) = parseExpr xs t in 
  let (ifstmts, rest') = parseStmts rest t in 
  if head rest' == K "else" then
    let (elsestmts, rest'') = parseStmts (tail $ tail rest') t in 
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
  else (expr++("if-goto TRUE"++q):("goto FALSE"++q):("label TRUE"++q):ifstmts++["label FALSE"++q], rest)
parseIf xs t = ([], xs)



  -- let (ss, xs', (gt', lt'))     = parseClassVarDecs xs v fn gt lt in
  -- let (ss', _, _) = parseFxnDecs xs v fn gt lt in 
  -- ss'

  -- let (s, i, x) = pa
  
  -- ["function "++cn++'.':id++' ':]


-- parseClassVarDecs :: [XMLNode] -> String -> String -> Table -> Table -> ([String], [XMLNode], (Table, Table))
-- parseClassVarDecs (K kind : typ : V v : C ';' : xs) cn fn gt lt =
   -- parseClassVarDecs xs cn fn (Map.insert v () gt )
-- parseClassVarDecs (K kind : typ : V v : C ';' : xs) cn fn gt lt =
-- parseClassVarDecs = undefined



-- xmlToVM xs v fn vm

-- xmlToVM (K "function" : typ : V name : C '(' : xs) cn fn vm = 
  -- let (ys, _:_:zs) = splitAbout (C ')') xs in   
  -- ("function "++cn++"."++name++' ':(show $ length ys)):xmlToVM zs cn name vm

-- xmlToVM (K "do" : V cn2 : C '.' : V fn2 : C '(' : xs) cn fn vm = 
  -- let (ys, _:zs) = splitAbout (C ';') xs in 
  -- shunt [] (init ys) ++
  -- ("call "++cn2++"."++fn2++" 1"):"pop temp 0":xmlToVM zs cn fn vm
-- xmlToVM (K "var" : typ : xs) cn fn vm = 
  -- let (ys, _:zs) = splitAbout (C ';') xs in 
  -- let vars = splitOn (C ',') ys in 
  -- xmlToVM xs cn fn (mapAll vm "local" vars 0)
  
-- xmlToVM (K "return" : C ';' : xs) cn fn gt = "push constant 0":"return":xmlToVM xs cn fn vm
-- xmlToVM (C '}' : xs) cn fn = xmlToVM xs cn fn vm
-- xmlToVM (K 'if' : C '(' : rest) cn fn = 


mapAll :: Table -> String -> [String] -> Int -> Table
mapAll vm _ [] _ = vm 
mapAll vm s (x:xs) i = undefined -- mapAll (insert x (s, i) vm) s xs (i+1)

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
  

  

  

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c [] = []
splitOn c s = go s [] 
  where go [] z     = [reverse z]
        go (y:ys) z = if c==y then reverse z : go ys [] else go ys (y:z)




-- parseExprs :: [XMLNode] -> ([String], [XMLNode])
-- parseExprs (xs) 


splitAbout :: Eq a => a -> [a] -> ([a], [a])
splitAbout x xs = (takeWhile (/=x) xs, dropWhile (/=x) xs)


parseLet :: [XMLNode] -> Table -> ([String], [XMLNode])
parseLet (K "let" : V v : C '=' : xs) t =
  let (s, x) = parseExpr xs t in 
  let (ty, kn, i) = t Map.! v in 
  (s++[("pop " ++ parseKind kn ++ ' ':show i)], tail x)






parseReturn :: [XMLNode] -> Table -> ([String], [XMLNode])
parseReturn (K "return" : C ';' : xs) _ = (["push constant 0", "return"], xs)
parseReturn (K "return" : xs) t = let (s, x) = parseExpr xs t in (s++["return"], tail xs)

parseExpr :: [XMLNode] -> Table -> ([String], [XMLNode])
parseExpr xs t = let (s, x:xs') = parseTerm xs t in case x of 
  C o | elem o "+-/*<>=|&" -> let (s', xs'') = parseTerm xs' t in (s++s'++[opToVM (C o)], xs'')
  _ -> (s, x:xs')

parseExprList :: [XMLNode] -> Table -> ([String], Int, [XMLNode])
parseExprList [] _ = ([], 0, [])
parseExprList (C ')' : xs) _ = ([], 0, xs)
parseExprList a@(x : xs) t = 
  let (expr, rest) = parseExpr (if x==C ',' then xs else a) t in 
  let (s, i, x)    = parseExprList rest t in 
  (expr++s, i+1, x)

parseTerm :: [XMLNode] -> Table -> ([String], [XMLNode])
parseTerm [] _ = ([], [])

parseTerm (N n : xs) t = (["push constant "++show n], xs)

parseTerm (S s : xs) t = 
  let ss = parseString s charToInt in (("push constant "++show (length s)):"call String.new 1":ss, xs)
  

parseTerm (K "true":xs) t =  (["push constant 0", "not"], xs)
parseTerm (K "false":xs) t = (["push constant 0" ], xs)
parseTerm (K "null":xs) t = (["push constant 0" ], xs)
parseTerm (K "this":xs) t = (["push pointer 0" ], xs)

parseTerm (C '(' : C '-' : xs) t = let (s, C ')' : xs') = parseExpr xs t in (s++["neg"], xs')
parseTerm (C '(' : C '~' : xs) t = let (s, C ')' : xs') = parseExpr xs t in (s++["not"], xs')

parseTerm (V cid : C '.' : V fid : C '(' : xs) t = 
  let (s, i, x) = parseExprList xs t in 
  (s++["call "++cid++"."++fid++" "++show i], x)
parseTerm (V id : xs) t = let (typ, kind, i) = t Map.! id in (["push "++(parseKind kind)++' ':show i], xs)

parseTerm (C '(' : xs) t = let (s,_:x) = parseExpr xs t in (s,x)
parseTerm (_:xs) _ = ([],xs)


parseKind :: Kind -> String
parseKind Static = "static"
parseKind Field  = "field"
parseKind Arg    = "argument"
parseKind Var    = "local"

parseString :: String -> Map.Map Char Int -> [String]
parseString "" _      = []
parseString (c:s) cti = ("push constant " ++ show (cti Map.! c)) : "call String.appendChar 2" : parseString s cti

  
-- parseStmt :: [XMLNode] -> Table -> ([String], [XMLNode])
-- parseStmt [] _ = ([], [])
-- parseStmt (K "return" : C ';' : xs) t = (["push constant 0", "return"], xs)
-- parseStmt (K "return" : xs) t = 
  -- let (expr, rest) = parseExpr xs t in (expr++["push constant 0", "return"], rest)
-- parseStmt (K "do" : V cn2 : C '.' : V fn2 : C '(' : xs) t = 
  -- let (ys, _:zs) = splitAbout (C ';') xs in 
  -- let (ys', i, zs') = parseExprList (init ys) t in
  -- (ys'++
  -- ["call "++cn2++"."++fn2++' ':(show $ i+(if ys/=[]then 1else 0)),
  -- "pop temp 0"], zs) 
-- parseStmt (K "if" : C '(' : xs) t = 
  -- let q = show $ length xs in 
  -- let (expr, C ')': C '{' : rest) = parseExpr xs t in 
  -- let (ifstmts, C '}':rest') = parseStmts rest t in 
  -- if head rest' == K "else" then
    -- let (elsestmts, C '}':rest'') = parseStmts $ tail $ tail rest' in 
    -- (expr++
    -- ("if-goto TRUE"++q):
    -- ("goto FALSE"++q):
    -- ("label TRUE"++q):
    -- ifstmts++
    -- ("label FALSE"++q):
    -- elsestmts,
    -- rest'')
  -- else (expr++("if-goto TRUE"++q):("goto FALSE"++q):("label TRUE"++q):ifstmts++["label FALSE"++q], rest)



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
