
import System.Random
import System.IO
import Data.Char

main::IO()
main=do
   f <- readFile "testFile.c"
   tablaSimb <- readFile "TablaDeSimbolos.txt"
   --print(tablaSimb)
   let tuplasTablaSimbolos = convertirTablaEnTuplas(tablaSimb)
   --print(tuplasTablaSimbolos)
   let str = "************* Analisis Lexico ************\n"
   writeFile "ResultadoAnalisis.txt" (str)
   --appendFile "ResultadoAnalisis.txt" (f ++ "\n")   
   let f1 = quitarIncludes(f)
   let f2 = quitarComentarioM(f1)
   let f3 = quitarComentarioL(f2)
   let pb = sacarPalabras(f3,[])
   --print(f3)
   print(pb)


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

quitarComentarioM::[Char]->[Char]
quitarComentarioM lista = do
	let list = take 2 lista
	if (null(lista)) then []
	else if (list=="/*") then buscarCierreComentarioM(drop 2 lista)
	else [head(lista)] ++ quitarComentarioM(tail(lista)) 

buscarCierreComentarioM::[Char]->[Char]
buscarCierreComentarioM lista = do
	let list= take 2 lista
	if (null(lista)) then []
	else if (list=="*/") then quitarComentarioM(drop 2 lista)
	else buscarCierreComentarioM(tail(lista))
	
quitarComentarioL::[Char]->[Char]
quitarComentarioL lista = do
	let list = take 2 lista
	if (null(lista)) then []
	else if (list=="//") then buscaCierreComentarioL(drop 2 lista)
	else [head(lista)] ++ quitarComentarioL(tail(lista))
	
buscaCierreComentarioL::[Char]->[Char]
buscaCierreComentarioL lista = do
	let list = take 1 lista
	if (null(lista)) then []
	else if (list=="\n") then quitarComentarioL(drop 1 lista)
	else buscaCierreComentarioL(tail(lista))
	
sacarPalabras::([Char],[Char])->[Char]
sacarPalabras (lista,l) = do
	let list = take 1 lista
	if (null(lista)) then [] 
	else if ((list>="a" && list<="z") || (list>="A" && list<="Z") || ((null(l))==False && list>="0" && list<="9")) then sacarPalabras(tail(lista),(l++list))
	else if ((list==" " || list=="\n" || list=="\t") && null(l)==True) then sacarPalabras(tail(lista),[])
	else if ((list==" " || list=="\n" || list=="\t") && null(l)==False) then l ++ [','] ++ sacarPalabras(tail(lista),[])
	else if (null(l)==False) then l ++ [','] ++ sacarSimbolos(lista,[])
	else sacarSimbolos(lista,[])
	
sacarSimbolos::([Char],[Char])->[Char]
sacarSimbolos (lista,l) = do
	let list = take 1 lista
	if (null(lista)) then []
	else if ((list==" " || list=="\n" || list=="\t") && null(l)==True) then sacarSimbolos(tail(lista),[])
	else if ((list==" " || list=="\n" || list=="\t") && null(l)==False) then l ++ [','] ++ sacarSimbolos(tail(lista),[])
	else if (list=="&" || list =="|") then sacarSimbolos(tail(lista),(l++list))
	else if ((list < "a" || list > "z") && (list<"A" || list>"Z")) then list ++ [','] ++ sacarSimbolos(tail(lista),[])
	else if (null(l)==False) then l ++ [','] ++ sacarPalabras(lista,[])
	else sacarPalabras(lista,[])