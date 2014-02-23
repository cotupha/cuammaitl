{-|
Módulo encargado de leer el archivo en el que se encuentra
la definición del modelo y la fórmula.
Parsea cada una de las partes involucradas.
-}
module Lector (
	leeArchivo
)
where

import SintaxisCTL
import IO
import Modelo
import Data.Set (Set)
import qualified Data.Set as Set

{-|
Lee el archivo cuyo nombre recibe como argumento.
Asume que la información en el archivo se encuentra de la siguiente manera:
* Conjunto de estados
* Conjunto de átomos
* Relación de transición
* Función de etiquetamiento
* Fórmula
Regresa la tupla (modelo, fórmula)
-}
leeArchivo :: FilePath -> IO (Modelo, CTL)                      
leeArchivo filename =

	bracket (openFile filename ReadMode) hClose
	        (\h -> do estados <- hGetLine h
	                  átomos <- hGetLine h
	                  relación <- hGetLine h
	                  etiquetas <- hGetLine h
	                  fórmula <- hGetLine h
	                  return (
	                  	(Modelo 
	                  		(leeConjunto estados)
	                  		(leeConjunto átomos)
	                  		(leeRelación relación)
	                  		(leeEtiquetas etiquetas)
	                  	 ),
	                  	(leeFórmula fórmula)
	                   )
	         )

{-|
Lee y parsea una cadena de la forma
{a,b,c,d,e}.
Regresa el conjunto (Set) obtenido de la cadena.
-}
leeConjunto :: String -> (Set NombreEstado)
leeConjunto conjunto = fst(head(leeConjuntoAux conjunto))

{-|
Lee y parsea una cadena de la forma
{a,b,c,d,e}
Regresa una lista tuplas (conjuntos, String), donde la cadena siempre
se espera vacía, porque es lo que le falta por analizar.
-}
leeConjuntoAux :: String -> [((Set NombreEstado), String)]
leeConjuntoAux s = [
		(Set.fromList xs, d) | ("{",a) <- lex s,
							(xs, c) <- leeElementos a,
							("}",d) <- lex c
		]

{-|
Lee y parsea una cadena de la forma
a,b,c,d,e
Regresa una lista tuplas (NombreEstado, String), donde la cadena siempre
se espera vacía, porque es lo que le falta por analizar.
-}
leeElementos :: String -> [([NombreEstado], String)]
leeElementos s = [
		(x:c, d)	|	(x, a) <-lex s,
					(",", b) <- lex a,
					(c, d) <- leeElementos b
		]
		++
		[
		([x],a)		| (x,a) <-lex s
		]

{-|
Lee y parsea una cadena de la forma
#s>{a,b,c}#s1>{b,d}
Regresa la función de etiquetamiento que dicha cadena representa.
-}		
leeEtiquetas :: String -> (Set Etiquetamiento)
leeEtiquetas etiquetas = Set.fromList(fst(head(leeEtiquetasAux etiquetas)))
		
{-|
Lee y parsea una cadena de la forma
#s>{a,b,c}#s1>{b,d}
Regresa una lista tuplas (Etiquetamiento, String), donde la cadena es lo
que le falta por analizar.
-}
leeEtiquetasAux :: String -> [([Etiquetamiento], String)]
leeEtiquetasAux s = [
		((Etiqueta a d):i, j)	| ("#", f) <- lex s,
							(a,b) <- lex f,
							(">",c) <- lex b,
							(d, e) <- leeConjuntoAux c,
							(i, j) <- leeEtiquetasAux e
		]
		++
		[
		([(Etiqueta a d)], e)	| ("#", f) <- lex s,
							(a,b) <- lex f,
							(">",c) <- lex b,
							(d, e) <- leeConjuntoAux c
		]

{-|
Lee y parsea una cadena de la forma
{(a,b),(c,d),(e,f)}
Regresa la relación de transición que dicha cadena representa.
-}		
leeRelación :: String -> (Set(String, String))
leeRelación relación = fst(head(leeRelaciónAux relación))
		
{-|
Lee y parsea una cadena de la forma
{(a,b),(c,d),(e,f)}
Regrega una lista de tuplas (Relación, String) donde la cadena siempre
se espera vacía, porque es lo que le falta por analizar.
-}
leeRelaciónAux :: String -> [(Set (String, String), String)]
leeRelaciónAux s = [
		(Set.fromList xs, d) | ("{",a) <- lex s,
							(xs, c) <- leeElementosRelación a,
							("}",d) <- lex c
		]

{-|
Lee y parsea una cadena de la forma
(a,b),(c,d),(e,f)
Regrega una lista de tuplas ([(String, String)], String) donde la cadena siempre
se espera vacía, porque es lo que le falta por analizar.
-}
leeElementosRelación :: String -> [([(String, String)], String)]		
leeElementosRelación s = [
		((b,e):i, j)	| ("(", a) <- lex s,
					(b, c) <- lex a,
					(",",d) <- lex c,
					(e, f) <- lex d,
					(")", g) <- lex f,
					(",",h) <- lex g,
					(i, j) <- leeElementosRelación h
		]
		++
		[
		([(b,e)], g)	| ("(", a) <- lex s,
					(b, c) <- lex a,
					(",",d) <- lex c,
					(e, f) <- lex d,
					(")", g) <- lex f
		]

{-|
Lee y parsea fórmulas en CTL.
Todas las subfórumlas deben estar entre paréntesis y aquéllas que tienen
dos partes, llevan el operador al principio.
Por ejemplo: (And(Lit r)(Lit q))
Regresa la fórmula en CTL
-}	                  
leeFórmula :: String -> CTL
leeFórmula fórmula = fst(head(leeFórmulaAux fórmula))

{-|
Lee y parsea fórmulas en CTL.
Regresa una lista tuplas (CTL, String), donde la cadena es lo
que le falta por analizar.
-}
leeFórmulaAux :: String -> [(CTL, String)]	
leeFórmulaAux s	=   [
			(Top, x) | 	("(", b) <- lex s,
						("Top", c) <- lex b,
						(")",	x) <- lex c
			]
			++
			[
			(Bot, x) | 	("(", b) <- lex s,
						("Bot", c) <- lex b,
						(")",	x) <- lex c
			]
			++
			[(Lit a, x) | ("(", b) <- lex s,
						 ("Lit", c) <- lex b,
						 (a, u) <- lex c,
						 (")",	x) <- lex u
			]
		   ++
		   [
		   	(Not a, x) | ("(", b) <- lex s,
						 ("Not", c) <- lex b,
						 (a, u) <- leeFórmulaAux c,
						 (")",	x) <- lex u
			]
		   ++
		   [
		   	(And a b, x) | ("(", c) <- lex s,
						   ("And", d) <- lex c,
						   (a, e) <- leeFórmulaAux d,
						   (b, f) <- leeFórmulaAux e,
						   (")",	x) <- lex f
		   ]
		   ++
		   [
		   	(Or a b, x) | ("(", c) <- lex s,
						   ("Or", d) <- lex c,
						   (a, e) <- leeFórmulaAux d,
						   (b, f) <- leeFórmulaAux e,
						   (")",	x) <- lex f
		   ]
		   ++
		   [
		   	(Then a b, x) | ("(", c) <- lex s,
						   ("Then", d) <- lex c,
						   (a, e) <- leeFórmulaAux d,
						   (b, f) <- leeFórmulaAux e,
						   (")",	x) <- lex f
		   ]
		   ++
		   [
		   	(EX a, x) | ("(", b) <- lex s,
						 ("EX", c) <- lex b,
						 (a, u) <- leeFórmulaAux c,
						 (")",	x) <- lex u
		   ]
		   ++
		   [
		   	(AF a, x) | ("(", b) <- lex s,
						 ("AF", c) <- lex b,
						 (a, u) <- leeFórmulaAux c,
						 (")",	x) <- lex u
		   ]
		   
		   ++
		   [
		   	(EU a b, x) | ("(", c) <- lex s,
						   ("EU", d) <- lex c,
						   (a, e) <- leeFórmulaAux d,
						   (b, f) <- leeFórmulaAux e,
						   (")",	x) <- lex f
		   ]
		   ++
		   [
		   	(AX a, x) | ("(", b) <- lex s,
						 ("AX", c) <- lex b,
						 (a, u) <- leeFórmulaAux c,
						 (")",	x) <- lex u
		   ]
		   ++
		   [
		   	(EF a, x) | ("(", b) <- lex s,
						 ("EF", c) <- lex b,
						 (a, u) <- leeFórmulaAux c,
						 (")",	x) <- lex u
		   ]
		   ++
		   [
		   	(AU a b, x) | ("(", c) <- lex s,
						   ("AU", d) <- lex c,
						   (a, e) <- leeFórmulaAux d,
						   (b, f) <- leeFórmulaAux e,
						   (")",	x) <- lex f
		   ]
		   ++
		   [
		   	(EG a, x) | ("(", b) <- lex s,
						 ("EG", c) <- lex b,
						 (a, u) <- leeFórmulaAux c,
						 (")",	x) <- lex u
		   ]
		   ++
		   [
		   	(AG a, x) | ("(", b) <- lex s,
						 ("AG", c) <- lex b,
						 (a, u) <- leeFórmulaAux c,
						 (")",	x) <- lex u
		   ]