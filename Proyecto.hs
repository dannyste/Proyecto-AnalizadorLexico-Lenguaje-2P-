
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
   let arch = quitarIncludes(f)
   print(arch)
   
quitarIncludes::[Char]->[Char]
quitarIncludes lista = do
		let nLista = removeSpaces(lista)
		if(null(nLista)) then nLista else if(head(nLista)=='#') then buscaInclude(nLista)
											 else nLista
					
buscaInclude:: [Char]->[Char]
buscaInclude lista = do
	let word = take 8 lista
	if(word=="#include") then quitaLinea(drop 8 lista)
						 else tail(lista)
	
quitaLinea:: [Char]->[Char]
quitaLinea lista = if(head(lista)=='\n') then quitarIncludes(tail(lista)) else quitaLinea(tail(lista))

removeSpaces::[Char]->[Char]
removeSpaces list = if (head(list)==' ' || head(list)=='\n') then removeSpaces(tail(list)) else list