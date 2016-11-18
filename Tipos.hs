module Tipos(
Nome(..),
Escopo(..),
Simbolo(..),
TabelaDeSimbolos(..),
Valor(..),
Tipo(..),
getValorPolinomio,
getValorMatriz,
getValorChar,
getValorString,
getValorBool,
getValorFloat,
getValorInt,
getValorDouble
)where

import Data.List
import Data.Array

data Valor = Polinomio_v (Array Int Double) | Matriz_v (Array (Int,Int) Double) | Char_v Char | String_v String | Bool_v Bool | Int_v Int | Double_v Double | Float_v Float | NULL deriving (Eq,Show)

data Simbolo =  Simbolo{nome::Nome,tipo::Tipo,valor::Valor,escopo::Escopo} deriving (Eq,Show)

type Nome = String
type Escopo = String

data Tipo = Int | Float | Double | Matriz | Polinomio | Char | Bool | String | SEMTIPO deriving (Eq,Show)

type TabelaDeSimbolos = [Simbolo]

getValorPolinomio::Valor -> (Array Int Double)
getValorPolinomio (Polinomio_v valor) = valor

getValorMatriz::Valor-> (Array (Int,Int) Double)
getValorMatriz (Matriz_v valor) = valor

getValorChar::Valor -> Char
getValorChar (Char_v valor) = valor

getValorString::Valor -> String
getValorString (String_v valor) = valor

getValorBool::Valor -> Bool
getValorBool (Bool_v valor) = valor

getValorInt::Valor -> Int
getValorInt (Int_v valor) = valor

getValorFloat::Valor -> Float
getValorFloat (Float_v valor) = valor

getValorDouble::Valor -> Double
getValorDouble (Double_v valor) = valor