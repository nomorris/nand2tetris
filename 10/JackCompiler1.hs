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

data XMLNode where 
  C :: Char   -> XMLNode
  K :: String -> XMLNode
  N :: Int    -> XMLNode
  S :: String -> XMLNode
  V :: String -> XMLNode  
  deriving (Show, Eq)

parseExpr :: [XMLNode] -> [XMLNode] -> [String]
parseExpr os [] = map opToVM os 
parseExpr os ((C ')'):xs) = 
  let (ps,qs) = splitAbout (C '(') os 
  in (map opToVM ps) ++ parseExpr (if qs==[] then [] else tail qs) xs 
parseExpr os ((C o):xs) | elem o "+-*/&|<>=(" = parseExpr (C o:os) xs 
parseExpr os (c:xs) = vmConst c : parseExpr os xs

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
  

  
translate :: String -> IO ()
translate s = do
  case s of
    inputFile -> do
      content <- readFile inputFile
      let outputFile = (takeWhile (/='.') inputFile)++".vm"
      let xml = jackToXML $ wordsWStrs $ splitter $ lines content
      let vm = xmlToVM xml "" "" 
      writeFile outputFile (unlines vm) 
      putStrLn $ "File '" ++ inputFile ++ "' has been written to '" ++ outputFile ++ "'."



xmlToVM :: [XMLNode] -> String -> String -> [String]
xmlToVM [] _ _ = []
xmlToVM (K "class" : V v : C '{' : xs) _ fn = xmlToVM xs v fn 
xmlToVM (K "function" : typ : V name : C '(' : xs) cn fn = 
  let (ys, _:_:zs) = splitAbout (C ')') xs in 
  ("function "++cn++"."++name++' ':(show $ length ys)):xmlToVM zs cn name 
xmlToVM (K "do" : V cn2 : C '.' : V fn2 : C '(' : xs) cn fn = 
  let (ys, _:zs) = splitAbout (C ';') xs in 
  parseExpr [] (init ys) ++
  ("call "++cn2++"."++fn2++" 1"):"pop temp 0":xmlToVM zs cn fn 
xmlToVM (K "return" : C ';' : xs) cn fn = "push constant 0":"return":xmlToVM xs cn fn
xmlToVM (C '}' : xs) cn fn = xmlToVM xs cn fn


  

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c [] = []
splitOn c s = go s [] 
  where go [] z     = [reverse z]
        go (y:ys) z = if c==y then reverse z : go ys [] else go ys (y:z)




splitAbout :: Eq a => a -> [a] -> ([a], [a])
splitAbout x xs = (takeWhile (/=x) xs, dropWhile (/=x) xs)











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


symbols = [[x] | x <- "<>&{}()[].,;+*/|~=-"]
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
