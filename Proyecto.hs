
import System.Random
import System.IO
import Data.Char

main::IO()
main=do
   f <- readFile "testFile.c"
   tablaSimb <- readFile "TablaDeSimbolos.txt"
   --print(tablaSimb)
   let tuplasTablaSimbolos = convertirTablaEnTuplas(tablaSimb)
   print(tuplasTablaSimbolos)
   let str = "************* Analisis Lexico ************\n"
   writeFile "ResultadoAnalisis.txt" (str)
   print(str)
   print(f)
   appendFile "ResultadoAnalisis.txt" (f ++ "\n")   
   let arch = quitarIncludes(f)
   print(arch)


convertirTablaEnTuplas::[Char]->[([Char],[Char])]
convertirTablaEnTuplas lista = do
	let tuplaToken = findFST(lista,"")
	let tuplaLexeme = findSND(snd(tuplaToken),"")
	if (null(lista)) then [] 
	else [(fst(tuplaToken),fst(tuplaLexeme))]++convertirTablaEnTuplas(snd(tuplaLexeme))


findFST::([Char],[Char])->([Char],[Char])
findFST (lista,tk) = do
	let token = tk ++ take 1 lista
	if(head(lista)==',') then (init token,tail(lista))
	else findFST(tail(lista),token)

findSND::([Char],[Char])->([Char],[Char])
findSND (lista,lex) = do
	let lexeme = lex ++ take 1 lista
	if(head(lista)=='\n') then (init lexeme,tail(lista))
	else findSND(tail(lista),lexeme)
   
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