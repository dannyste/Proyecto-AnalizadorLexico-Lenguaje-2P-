
import System.Random
import System.IO
import Data.Char

main::IO()
main=do
   f <- readFile "testFile.c"
   let str = "************* Analisis Lexico ************\n"
   writeFile "ResultadoAnalisis.txt" (str)
   print(str)
   print(f)
   appendFile "ResultadoAnalisis.txt" (f ++ "\n")