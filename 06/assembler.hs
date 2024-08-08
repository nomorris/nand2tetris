import qualified Data.Map as Map
import System.IO (readFile, writeFile)
import System.FilePath
import Data.List
import Data.Maybe
import Data.Char
import Text.Read
import System.Environment

(//) = div
(%)  = mod

-- USAGE:
--   cd Downloads\nand2tetris\nand2tetris\projects\06
--   ghc --make -o assembler assembler.hs
--   ./assembler "pong\Pong.asm"

initMap :: Map.Map String Int 
initMap = Map.fromList (
  ("SP", 0):("LCL", 1):("ARG", 2):("THIS", 3):
  ("THAT", 4):("SCREEN", 16384):("KBD", 24576):
  (map (\x -> ('R':(show x), x)) [0..15]))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> assemble inputFile
    _ -> putStrLn "I don't want to do that one."

-- Main function
assemble :: String -> IO ()
assemble s = do
  case s of
    inputFile -> do
      content <- readFile inputFile
      let outputFile = replaceExtension inputFile ".hack"
      let newmap = getVars 0 (lines content) initMap
      -- print newmap
      let bits = interpret 16 newmap $ lines content
      writeFile outputFile $ unlines bits
      putStrLn $ "File '" ++ inputFile ++ "' has been written to '" ++ outputFile ++ "'."
      

getVars :: Int -> [String] -> Map.Map String Int -> Map.Map String Int
getVars c [] m = m
getVars c ([]:ls) m = getVars c ls m
getVars c (('/':'/':l):ls) m = getVars c ls m
getVars c (('\t':l):ls) m = getVars c (l:ls) m
getVars c (('\r':l):ls) m = getVars c (l:ls) m
getVars c ((' ':l):ls) m = getVars c (l:ls) m
getVars c (('(':l):ls) m = getVars c ls (Map.insert (takeWhile (/=')') l) c m)
getVars c (l:ls) m = getVars (c+1) ls m


interpret :: Int -> Map.Map String Int -> [String] -> [String]
interpret _ m []               = []
interpret n m ([]:ls)          = interpret n m ls
interpret n m (('/':'/':l):ls) = interpret n m ls
interpret n m (('(':l):ls)     = interpret n m ls
interpret n m ((' ':l):ls)     = interpret n m (l:ls)
interpret n m (('@':l):ls)     = 
  case (readMaybe l) of 
    Nothing -> 
      case Map.lookup (takeWhile (/=' ') l) m of
        Just addr -> decToBin addr 16 : interpret n m ls  
        Nothing   -> decToBin n 16 : interpret (n+1) (Map.insert l n m) ls 
    Just q -> decToBin q 16 : interpret n m ls 
interpret n m (l:ls) =  
  ('1':'1':'1':
  findABit (if elem '=' l then tail $ dropWhile (/='=') l else l):
  (aluBits $ takeWhile (/=';') (if elem '=' l then tail $ dropWhile (/='=') l else l))++
  (if elem '=' l then (destBits $ takeWhile (/='=') l) else "000")++
  (jumpBits $ dropWhile (/='J') l)):
  interpret n m ls

destBits :: String -> String
destBits l = (if elem 'A' l then '1' else '0'):
             (if elem 'D' l then '1' else '0'):
             (if elem 'M' l then '1' else '0'):[]

jumpBits :: String -> String 
jumpBits ('J':'M':'P':_) = "111"
jumpBits ('J':'L':'E':_) = "110"
jumpBits ('J':'N':'E':_) = "101"
jumpBits ('J':'L':'T':_) = "100"
jumpBits ('J':'G':'E':_) = "011"
jumpBits ('J':'E':'Q':_) = "010"
jumpBits ('J':'G':'T':_) = "001"
jumpBits _         = "000"


aluBits :: String -> String
aluBits s
 | s == "0"   = "101010"
 | s == "1"   = "111111"
 | s == "-1"  = "111010"
 | s == "D"   = "001100"
 | s == "M"   = "110000"
 | s == "A"   = "110000"
 | s == "!D"  = "001101"
 | s == "!A"  = "110001"
 | s == "!M"  = "110001"
 | s == "-D"  = "001111"
 | s == "-A"  = "110011"
 | s == "-M"  = "110011"
 | s == "D+1" = "011111"
 | s == "A+1" = "110111"
 | s == "M+1" = "110111"
 | s == "D-1" = "001110"
 | s == "A-1" = "110010"
 | s == "M-1" = "110010"
 | s == "D+A" = "000010"
 | s == "D+M" = "000010"
 | s == "D-A" = "010011"
 | s == "D-M" = "010011"
 | s == "A-D" = "000111"
 | s == "M-D" = "000111"
 | s == "D&A" = "000000"
 | s == "D&M" = "000000"
 | s == "D|A" = "010101"
 | s == "D|M" = "010101"
 | otherwise = "010101"

findABit :: String -> Char
findABit [] = '0'
findABit (';':_) = '0'
findABit ('A':_) = '0'
findABit ('M':_) = '1'
findABit (_:r) = findABit r

decToBin :: Int -> Int -> String
decToBin _ 0 = ""
decToBin n x = decToBin (n//2) (x-1) ++ if n%2==1 then "1" else "0"