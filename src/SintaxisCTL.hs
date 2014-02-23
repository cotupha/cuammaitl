{-|
Módulo en el que se define ĺa sintaxis de las
fórmulas de CTL que se podrán procesar.
-}
module SintaxisCTL(
	CTL(..)
	)
	where

{-|
Tipo de dato que representa a una fórmula en CTL.
-}
data CTL
	= Top			
	| Bot
	| Lit String	--Letras proposicionales
	| Not CTL		--No algo
	| And CTL CTL	--algo Y algo
	| Or CTL CTL	--algo O algo
	| Then CTL CTL	--algo -> algo
	--Comienza conjunto adecuado
	| EX CTL		--EX
	| AF CTL		--AF
	| EU CTL CTL	--E[algo U algo]
	--Otras que no son del conjunto adecuado
	| AX CTL
	| EF CTL
	| AU CTL CTL
	| EG CTL
	| AG CTL
	deriving(Show)