import System.IO (readFile, writeFile)
import System.FilePath
import System.Environment
import Data.List
import qualified Data.Map as Map


main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> translate inputFile
    _ -> putStrLn "I don't want to do that one."

data XMLNode where 
  C :: Char   -> XMLNode
  K :: String -> XMLNode
  N :: Int    -> XMLNode
  S :: String -> XMLNode
  V :: String -> XMLNode  
  deriving (Show, Eq)

type Kind = Static | Field | Arg | Var | None

type Table = Map String (String, String, Int)

translate :: String -> IO ()
translate s = do
  case s of
    inputFile -> do
      content <- readFile inputFile
      let outputFile = (takeWhile (/='.') inputFile)++"T.vm"
      let xml = jackToXML $ wordsWStrs $ splitter $ lines content
      let vm = xmlToVM xml "" "" 
      writeFile outputFile (show xml) 
      putStrLn $ "File '" ++ inputFile ++ "' has been written to '" ++ outputFile ++ "'."

compileWhile :: [XMLNode] -> Table -> ([String], [XMLNode])
compileWhile (K "while" : C '(' : xs) = 
  let (bool, rest) = compileExpr xs t in 
  let (stmts, rest') = compileStmts (drop 2 rest) t in
  (["label WHILET"++show(length xs)]++
   bool++
   ["not", "if-goto WHILEF"++show(length xs)]++
   ["goto WHILET"++show(length xs)]++
   ["label WHILEF"++show(length xs)])

compileDo :: [XMLNode] -> Table -> ([String], [XMLNode])
compileDo (K "do" : V cn : C '.' : V fn : C '(' : xs) t = 
  let (exprs, i, rest) = compileExprList xs t in 
  (exprs++["call "++cn++"."++fn++" "++show i, "pop temp 0"], rest)




compileReturn :: [XMLNode] -> Table -> ([String], [XMLNode])
compileReturn (K "return" : C ';' : xs) _ = (["push constant 0", "return"], xs)
compileReturn (K "return" : xs) t = let (s, x) = compileExpr xs t in (s++["return"], tail xs)

compileExpr :: [XMLNode] -> Table -> ([String], [XMLNode])
compileExpr xs t = let (s, x:xs') = compileTerm xs t in case x of 
  C o | elem o "+-/*<>=|&" -> let (s', xs'') = compileTerm xs' t in (s++s'++[opToVm (C o)], xs'')
  _ -> (s, x:xs')

compileExprList :: [XMLNode] -> Table -> ([String], Int, [XMLNode])
compileExprList [] _ = ([], 0, xs)
compileExprList (C ')' : xs) _ = ([], 0, xs)
compileExprList a@(x : xs)   t = 
  let (expr, rest) = compileExpr (if x==C ',' then xs else a) t in 
  let (s, i, x) = compileExprList rest t in 
  (expr++s, i+1, x)

compileTerm :: [XMLNode] -> Table -> ([String], [XMLNode])
compileTerm [] _ = []
compileTerm (N n : xs) t = (["push constant "++show n], xs)
compileTerm (K "true":xs) t =  (["push constant -1"], xs)
compileTerm (K "false":xs) t = (["push constant 0" ], xs)
compileTerm (C '(' : C '-' : xs) t = let (s, x:xs') = parseExpr xs t in (s++["neg"], xs')
compileTerm (C '(' : C '~' : xs) t = let (s, x:xs') = parseExpr xs t in (s++["not"], xs')
compileTerm (V cid : C '.' : V fid : C '(' : xs) t = 
  let (s, i, x) = compileExprList xs t in 
  (s++["call "++cid++"."++fid++" "++show i], x)
compileTerm (V id : xs) t = let (typ, kind, i) = t!id in ["push "++kind++' ':show i]

-- compileTerm (S s : xs) = 
-- compileTerm (V id : C '[' : xs) t = let (expr, xs') = compileExpr xs t in ()





xmlToVM :: [XMLNode] -> String -> String -> Table -> Table -> [String]
xmlToVM _ _ _ _ _ = []









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
longComment ((_:s):ss) = longComment (s:ss)


symbols  = [[x] | x <- "<>&{}()[].,;+*/|~=-"]
keywords = ["class","constructor","function","method","field","static","var","int","char","boolean","void","true","false","null","this","let","do","if","else","while","return"]


-- input split by word, output split by line.
jackToXML :: [String] -> [XMLNode]
jackToXML [] = []
jackToXML ([]:s) = jackToXML s
jackToXML (('"':l):s)                     = S l     : jackToXML s
jackToXML ((c:w):s) | elem [c] symbols    = C c     : jackToXML s
jackToXML (w:s)     | elem w keywords     = K w     : jackToXML s
jackToXML ((c:w):s) | elem c "1234567890" = N (read $ c:w) : jackToXML s
jackToXML (w:s)                           = V w     : jackToXML s