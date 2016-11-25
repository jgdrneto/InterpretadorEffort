module Tipos(
Nome(..),
Escopo(..),
Simbolo(..),
TabelaDeSimbolos(..),
Valor(..),
Tipo(..),
Comandos(..),
getValorPolinomio,
getValorMatriz,
getValorChar,
getValorString,
getValorBool,
getValorFloat,
getValorInt,
getValorDouble,
conversorStringParaTipo
)where

import Data.List
import Data.Array

data Valor = Polinomio_v (Array Int Double) | Matriz_v (Array (Int,Int) Double) | Char_v Char | String_v String | Bool_v Bool | Int_v Int | Double_v Double | Float_v Float | NULL deriving (Eq,Ord,Show)

data Simbolo =  Simbolo{nome::Nome,tipo::Tipo,valor::Valor,escopo::Escopo} deriving (Eq,Show)

type Nome = String
type Escopo = String
type Comandos = [String]

data Tipo = Int | Float | Double | Matriz | Polinomio | Char | Bool | String | SEMTIPO deriving (Eq,Show)

type TabelaDeSimbolos = [Simbolo]

getValorPolinomio::Valor -> (Array Int Double)
getValorPolinomio (Polinomio_v valor) = valor
--getValorPolinomio _ = Array (1,1) [(1,0)]

getValorMatriz::Valor-> (Array (Int,Int) Double)
getValorMatriz (Matriz_v valor) = valor
--getValorMatriz _ = Array ((1,1),(1,1)) [((1,1),0)]

getValorChar::Valor -> Char
getValorChar (Char_v valor) = valor
getValorChar _ = ' '

getValorString::Valor -> String
getValorString (String_v valor) = valor
getValorString _ = " "


getValorBool::Valor -> Bool
getValorBool (Bool_v valor) = valor
getValorBool _ = False

getValorInt::Valor -> Int
getValorInt (Int_v valor) = valor
getValorInt _ = 0

getValorFloat::Valor -> Float
getValorFloat (Float_v valor) = valor
getValorFloat _ = 0.0

getValorDouble::Valor -> Double
getValorDouble (Double_v valor) = valor
getValorDouble _ = 0.0

conversorStringParaTipo :: String -> Tipo
conversorStringParaTipo "" = SEMTIPO
conversorStringParaTipo tipo = case tipo of
                               "Int" -> Int
                               "Float" -> Float
                               "Double" -> Double
                               "Matriz" -> Matriz
                               "Polinomio" -> Polinomio
                               "Char" -> Char
                               "Bool" -> Bool
                               "String"-> String