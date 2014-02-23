{-|
Módulo en el que se define la relación de transición.
-}
module Rela where

import Data.Set (Set)
import qualified Data.Set as Set

{-|
Definimos el tipo Relación como un conjunto de parejas
|-}
type Relación a = Set (a,a)

{-|
Obtiene la imagen de un elemento en la relación
|-}
image :: Ord a => Relación a -> a -> Set a
image rel val = Set.map snd (Set.filter ((==val).fst) rel)

{-|
Obtiene la preimagen de un elemento en la relación
|-}
preImagen :: Ord a => Relación a -> a -> Set a
preImagen relación valor = Set.map fst (Set.filter ((==valor).snd) relación)