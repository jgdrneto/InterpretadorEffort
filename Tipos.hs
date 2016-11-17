module Tipos(

)where

import Data.List
import Data.Array
import Data.Ix

-- Definindo tipos primitivos na nossa linguagem 
type Matriz = Array(Int,Int) Tipo
type Polinomio = Array Int Numero
type NULL = String

--Todos os tipos primitivos da nossa linguagem
data Tipo = Numero | Char | Matriz | Polinomio
data Numero = Int | Float | Double

--Definindo valores dos tipos
data Valor = StringV String | IntV Int | FloatV Float | DoubleV Double | CharV Char | MatrizV Matriz Int Int Tipo | PolinomioV Polinomio Int Numero | NULLV NULL 

--Construindo tabela de símbolos
type Nome = String
type Escopo = String
type Simbolo = (Nome, Tipo, [Valor], Escopo)
type TabelaDeSimbolos = [Simbolo]



