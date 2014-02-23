{-|
Módulo que se encarga de verificar un modelo y una
fórmula.
-}
module VerificadorCTL where

import Modelo
import SintaxisCTL
import Data.Set (Set)
import qualified Data.Set as Set
import Rela
import Lector

{-|
Se encarga de verificar el modelo y la fórmula especificados
en el archivo cuyo nombre recibe.
Imprime el conjunto de estados que satisfacen a la fórmula.
-}
verifica :: FilePath -> IO()
verifica archivo = do
	((Modelo estados átomos relación etiquetas), fórmulaCTL) <- (leeArchivo archivo)
	putStrLn (show (edosVálidos estados (obtenEtiquetamientoInverso (Modelo estados átomos relación etiquetas)) relación fórmulaCTL))

{-|
Obtiene los estados en los que es válida una fórmula dada.
También recibe los estados, la relación de transición y
la función de etiquetamiento inverso.
Regresa el conjunto de etados que satisfacen a la fórmula.
-}
edosVálidos														:: (Set NombreEstado) -> EtiquetamientoInverso -> (Relación NombreEstado) -> CTL -> (Set NombreEstado)
edosVálidos estados etiquetamientoInverso relación Top			= estados
edosVálidos estados etiquetamientoInverso relación Bot			= Set.empty
edosVálidos estados etiquetamientoInverso relación (Lit c)		= edosVálidosÁtomo etiquetamientoInverso c 
edosVálidos estados etiquetamientoInverso relación (Not p)		= estados `Set.difference` (edosVálidos estados etiquetamientoInverso relación p)
edosVálidos estados etiquetamientoInverso relación (Or p q)		= (edosVálidos estados etiquetamientoInverso relación p) `Set.union` (edosVálidos estados etiquetamientoInverso relación q)
edosVálidos estados etiquetamientoInverso relación (And p q)	= (edosVálidos estados etiquetamientoInverso relación p) `Set.intersection` (edosVálidos estados etiquetamientoInverso relación q)
edosVálidos estados etiquetamientoInverso relación (Then p q)	= (estados `Set.difference` (edosVálidos estados etiquetamientoInverso relación p)) `Set.union` (edosVálidos estados etiquetamientoInverso relación q)
edosVálidos estados etiquetamientoInverso relación (EX p)		= checkEX estados etiquetamientoInverso relación p
edosVálidos estados etiquetamientoInverso relación (AF p)		= checkAF estados etiquetamientoInverso relación p
edosVálidos estados etiquetamientoInverso relación (EU p q)		= checkEU estados etiquetamientoInverso relación p q
edosVálidos estados etiquetamientoInverso relación (AX p)		= edosVálidos estados etiquetamientoInverso relación (Not (EX (Not p)))
edosVálidos estados etiquetamientoInverso relación (EF p)		= edosVálidos estados etiquetamientoInverso relación (EU Top p)
edosVálidos estados etiquetamientoInverso relación (AU p q)		= edosVálidos estados etiquetamientoInverso relación (Not((EU (Not q)((Not p) `And` (Not q))) `Or` (EG(Not q))))
edosVálidos estados etiquetamientoInverso relación (EG p)		= edosVálidos estados etiquetamientoInverso relación (Not (AF (Not p)))
edosVálidos estados etiquetamientoInverso relación (AG p)		= edosVálidos estados etiquetamientoInverso relación (Not (EF (Not p)))

{-|
Función encargada de validar si el nombre del átomo
coincide con la cadena que recibe.
-}
encuentraÁtomo		:: Ato -> NombreÁtomo -> Bool
encuentraÁtomo (Átomo c edos) b
	| (c == b)		= True
	| otherwise		= False 

{-|
Dado un etiquetamiento inverso, obtiene los estados que satisfacen
al átomo que recibe como argumento.
-}	
edosVálidosÁtomo :: EtiquetamientoInverso -> NombreÁtomo -> (Set NombreEstado)
--Aquí da igual si utilizamos findMin o findMax, siempre tenemos sólo un elemento en ese conjunto
edosVálidosÁtomo etiquetamientoInverso c = satisfactibleEn(
						Set.findMin(
							Set.filter (\x->encuentraÁtomo x c) etiquetamientoInverso
						)
					  )

{-|
Obtiene la preImagenExistencial de un conjunto de estados,
basándose en la relación que recibe como argumento
-}					 
preImagenExistencial	:: (Relación NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado)
preImagenExistencial relación cjtoEstados=  
	Set.fold Set.union Set.empty (Set.map (\x -> preImagen relación x) cjtoEstados)

{-|
Obtiene la preImagenUniversal de un conjunto de estados,
basándose en la relación y el conunto de estados (todo S), que
recibe como argumento
-}
preImagenUniversal :: (Relación NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado)
preImagenUniversal relación estados cjtoEstados =
	estados `Set.difference` (preImagenExistencial relación (estados `Set.difference` cjtoEstados))
	 
{-|
Obtiene los estados en que es válido EX alfa
-}
checkEX :: (Set NombreEstado) -> EtiquetamientoInverso -> (Relación NombreEstado) -> CTL -> (Set NombreEstado)
checkEX estados etiquetamientoInverso relación p = preImagenExistencial relación (edosVálidos estados etiquetamientoInverso relación p)

{-|
Obtiene los estados en que es válido AF alfa
-}
checkAF :: (Set NombreEstado) -> EtiquetamientoInverso -> (Relación NombreEstado) -> CTL -> (Set NombreEstado)
checkAF estados etiquetamientoInverso relación p = checkAFAux relación estados estados (edosVálidos estados etiquetamientoInverso relación p)

{-|
Función auxiliar para encontrar los estados en que es
válido AF
(simula lo que está dentro del while)
-}	
checkAFAux :: (Relación NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado)
checkAFAux relación estados x y =
	if not ((x `Set.isSubsetOf` y) && (y `Set.isSubsetOf` x))
		then checkAFAux relación estados y (y `Set.union` (preImagenUniversal relación estados y))
		else y

{-|
Obtiene los estados en que es válido EU alfa
-}		
checkEU :: (Set NombreEstado) -> EtiquetamientoInverso -> (Relación NombreEstado) -> CTL -> CTL -> (Set NombreEstado)
checkEU estados etiquetamientoInverso relación p q = checkEUAux relación (edosVálidos estados etiquetamientoInverso relación p) estados (edosVálidos estados etiquetamientoInverso relación q)

{-|
Función auxiliar para encontrar los estados en que es
válido EU
(simula lo que está dentro del while)
-}
checkEUAux :: (Relación NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado)
checkEUAux relación w x y =
	if not ((x `Set.isSubsetOf` y) && (y `Set.isSubsetOf`x))
		then checkEUAux relación w y (y `Set.union` (w `Set.intersection` (preImagenExistencial relación y)))
		else y