module AvaliadorDeExpressoes(
obterValor
)where

import Tipos
import TabelaDeSimbolos
import Data.Array
import System.IO.Unsafe

obterValor:: Comandos -> TabelaDeSimbolos -> (Valor,Comandos)
obterValor [] _ = (NULL,[])
obterValor _ [] = (NULL,[])
obterValor cmds tbs = calcularExpressao NULL cmds tbs 


calcularExpressao :: Valor -> Comandos -> TabelaDeSimbolos -> (Valor,Comandos)
calcularExpressao NULL [] _ = (NULL,[])
calcularExpressao NULL (cab:cal) tbs = calcularExpressao (buscarValor tbs cab "main") cal tbs
calcularExpressao (Int_v v) (cab:cal) tbs =  case cab of
                                             ">" -> maior (Int_v v) cal tbs 
                                             ">=" -> ((Int_v v),cal) --maiorIgual valor cal tbs
                                             "<=" -> ((Int_v v),cal)--menorIgual valor cal tbs
                                             ")" -> ((Int_v v),cal)
                                             _ -> (NULL,cal)                                       
calcularExpressao (Float_v f) (cab:cal) tbs =  case cab of
                                             "+" ->  soma (Float_v f) cal tbs--soma valor cal tbs
                                             "-" ->  subtracao (Float_v f) cal tbs --unsafePerformIO (imprimirConsole (Float_v f) cal tbs) --
                                             "*" ->  multiplicacao (Float_v f) cal tbs--multiplicacao valor cal tbs
                                             ")" -> ((Float_v f),cal)
                                             _ -> (NULL,cal)
calcularExpressao (Matriz_v m) (cab:cal) tbs =  case cab of
                                             "+" -> ((Int_v 0),cal)--soma valor cal tbs
                                             "*" -> multiplicacao (Matriz_v m) cal tbs--multiplicacao valor cal tbs
                                             ")" -> ((Matriz_v m),cal)
                                             _ -> (NULL,cal)
calcularExpressao (Bool_v b) (cab:cal) tbs =  case cab of
                                             "&&" ->  ((Int_v 0),cal)--soma valor cal tbs
                                             ")" -> ((Bool_v b),cal)
                                             _ -> (NULL,cal)

imprimirConsole:: Valor -> Int -> String -> Comandos -> TabelaDeSimbolos -> IO (Valor,Comandos)  
imprimirConsole _ _ _[] _ = return (NULL,[])
imprimirConsole (Float_v f) i c cmds tbs = do
                                             print c
                                             print i
                                             print f
                                             print tbs
                                             return ((Float_v f),cmds)

maior:: Valor -> Comandos -> TabelaDeSimbolos -> (Valor,Comandos)
maior _ [] _ = (NULL,[])
maior (Int_v v) (cab:cal) tbs = let x = (v > (read cab ::Int)) in 
                                   if(cal==[]) then (NULL,cal)
                                   else
                                   calcularExpressao (Bool_v x) cal tbs  

multiplicacao :: Valor -> Comandos -> TabelaDeSimbolos ->(Valor,Comandos)
multiplicacao _ [] _ = (NULL,[])
multiplicacao (Matriz_v m) (cab:cal) tbs = ((Matriz_v (multiplicarMatriz m (buscarValor tbs cab "main"))),cal) 
multiplicacao (Float_v f) (cab:cal) tbs = let x = getValorFloat (buscarValor tbs cab "main") in
                                          if(cal==[]) then (NULL,cal)
                                          else calcularExpressao (Float_v (x*f)) cal tbs

soma :: Valor -> Comandos -> TabelaDeSimbolos -> (Valor,Comandos)
soma _ [] _  = (NULL, [])
soma (Float_v f) (cab:cal) tbs = let x = getValorInt (buscarValor tbs cab "main") in
                                      if(cal==[]) then (NULL,cal)
                                      else calcularExpressao (Float_v (f + (fromIntegral x))) cal tbs

subtracao :: Valor -> Comandos -> TabelaDeSimbolos -> (Valor, Comandos)
subtracao _ [] _ = (NULL, [])
subtracao (Float_v f) (cab:cal) tbs = let x = getValorFloat (buscarValor tbs cab "main") in
                                      if(cal==[]) then (NULL,cal)
                                      else calcularExpressao (Float_v (f - x)) cal tbs --unsafePerformIO (imprimirConsole (Float_v f) x cab cal tbs)

multiplicarMatriz :: Array (Int,Int) Double -> Valor -> Array (Int,Int) Double
multiplicarMatriz m (Matriz_v n) = m
multiplicarMatriz m _ = m
                            
eOperadorLogico :: String -> Bool
eOperadorLogico "" = False
eOperadorLogico s = if (s=="||" || s=="&&" || s =="<=" || s==">="|| s== "==" || s=="!=" || s == ">" || s == "<" || s== "!") then True
              else False

eOperadorAritmetico :: String -> Bool
eOperadorAritmetico "" = False
eOperadorAritmetico s = if(s=="+" || s=="-" || s=="*" ) then True
                        else False