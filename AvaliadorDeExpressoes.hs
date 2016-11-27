module AvaliadorDeExpressoes(
obterValor
)where

import Tipos
import TabelaDeSimbolos
import Data.Array
import System.IO
import System.IO.Unsafe
import Data.Matrix

obterValor:: Comandos -> TabelaDeSimbolos -> (Valor,Comandos)
obterValor [] _ = (ERRO,[])
obterValor cmds tbs = calcularExpressao NULL cmds tbs 


calcularExpressao :: Valor -> Comandos -> TabelaDeSimbolos -> (Valor,Comandos)
calcularExpressao _ [] _ = (ERRO,[])
calcularExpressao ERRO cmds _ = (ERRO,cmds)
calcularExpressao NULL (cab:cal) tbs = calcularExpressao (buscarValor tbs cab "main") cal tbs
calcularExpressao (Int_v v) (cab:cal) tbs =  case cab of
                                             ">" -> maior (Int_v v) cal tbs 
                                             ">=" -> maiorIgual (Int_v v) cal tbs --maiorIgual valor cal tbs
                                             "<=" -> menorIgual (Int_v v) cal tbs--menorIgual valor cal tbs
                                             ")" -> ((Int_v v),cal)
                                             _ -> (ERRO,cal)                                       
calcularExpressao (Float_v f) (cab:cal) tbs =  case cab of
                                             "+" ->  soma (Float_v f) cal tbs--soma valor cal tbs
                                             "-" ->  subtracao (Float_v f) cal tbs --unsafePerformIO (imprimirConsole (Float_v f) cal tbs) --
                                             "*" ->  multiplicacao (Float_v f) cal tbs--multiplicacao valor cal tbs
                                             ")" -> ((Float_v f),cal)
                                             _ -> (ERRO,cal)
calcularExpressao (Matriz_v m) (cab:cal) tbs =  case cab of
                                             "+" -> soma (Matriz_v m) cal tbs--soma valor cal tbs
                                             "*" -> multiplicacao (Matriz_v m) cal tbs--multiplicacao valor cal tbs
                                             ")" -> ((Matriz_v m),cal)
                                             _ -> (ERRO,cal)
calcularExpressao (Bool_v b) (cab:cal) tbs =  case cab of
                                             "&&" ->  operadorEE (Bool_v b) cal tbs--soma valor cal tbs
                                             ")" -> ((Bool_v b),cal)
                                             _ -> (ERRO,cal)
calcularExpressao _ (cab:cal) tbs = (ERRO,(cab:cal))

imprimirConsole:: Valor -> Int -> String -> Comandos -> TabelaDeSimbolos -> IO (Valor,Comandos)  
imprimirConsole _ _ _[] _ = return (NULL,[])
imprimirConsole (Float_v f) i c cmds tbs = do
                                             print c
                                             print i
                                             print f
                                             print tbs
                                             return ((Float_v f),cmds)

operadorEE:: Valor -> Comandos -> TabelaDeSimbolos -> (Valor,Comandos)
operadorEE ERRO cmds _ = (ERRO,cmds)
operadorEE (Bool_v b) cmds tbs = let (v,cal) = calcularExpressao NULL cmds tbs in
                                 let (Bool_v bool) = v in
                                 if ( (bool && b) == True ) then ((Bool_v True),cal)
                                 else ((Bool_v False),cal)  

maior:: Valor -> Comandos -> TabelaDeSimbolos -> (Valor,Comandos)
maior _ [] _ = (NULL,[])
maior (Int_v v) (cab:cal) tbs = let x = (v > (read cab ::Int)) in 
                                   if(cal==[]) then (ERRO,cal)
                                   else
                                   calcularExpressao (Bool_v x) cal tbs  

maiorIgual:: Valor -> Comandos -> TabelaDeSimbolos -> (Valor,Comandos)
maiorIgual _ [] _ = (NULL,[])
maiorIgual (Int_v v) (cab:cal) tbs = let x = (v >= (read cab ::Int)) in 
                                   if(cal==[]) then (ERRO,cal)
                                   else
                                   calcularExpressao (Bool_v x) cal tbs

menorIgual:: Valor -> Comandos -> TabelaDeSimbolos -> (Valor,Comandos)
menorIgual _ [] _ = (NULL,[])
menorIgual (Int_v v) (cab:cal) tbs = let x = (v <= (read cab ::Int)) in 
                                   if(cal==[]) then (ERRO,cal)
                                   else
                                   calcularExpressao (Bool_v x) cal tbs

multiplicacao :: Valor -> Comandos -> TabelaDeSimbolos ->(Valor,Comandos)
multiplicacao _ [] _ = (NULL,[])
multiplicacao (Matriz_v m) (cab:cal) tbs = let n = getValorMatriz(buscarValor tbs cab "main") in
                                           if (ncols m /= nrows n ) then calcularExpressao (unsafePerformIO(erroMulMatriz)) cal tbs
                                           else calcularExpressao (Matriz_v (m*n)) cal tbs
multiplicacao (Float_v f) (cab:cal) tbs = let x = getValorFloat (buscarValor tbs cab "main") in
                                          if(cal==[]) then (ERRO,cal)
                                          else calcularExpressao (Float_v (x*f)) cal tbs

soma :: Valor -> Comandos -> TabelaDeSimbolos -> (Valor,Comandos)
soma _ [] _  = (NULL, [])
soma (Float_v f) (cab:cal) tbs = let x = getValorInt (buscarValor tbs cab "main") in
                                 if(cal==[]) then (ERRO,cal)
                                 else calcularExpressao (Float_v (f + (fromIntegral x))) cal tbs
soma (Matriz_v m) (cab:cal) tbs = let n = getValorMatriz (buscarValor tbs cab "main") in
                                  if(nrows m == nrows n && ncols m == ncols n) then calcularExpressao (Matriz_v (m+n)) cal tbs
                                  else calcularExpressao (unsafePerformIO(erroSomaMatriz)) cal tbs


subtracao :: Valor -> Comandos -> TabelaDeSimbolos -> (Valor, Comandos)
subtracao _ [] _ = (NULL, [])
subtracao (Float_v f) (cab:cal) tbs = let x = getValorFloat (buscarValor tbs cab "main") in
                                      if(cal==[]) then (ERRO,cal)
                                      else calcularExpressao (Float_v (f - x)) cal tbs --unsafePerformIO (imprimirConsole (Float_v f) x cab cal tbs)

erroMulMatriz:: IO(Valor)
erroMulMatriz = do
                  putStrLn "ERRO: Número de linhas diferente do número de colunas"
                  return ERRO  
      
erroSomaMatriz:: IO(Valor)
erroSomaMatriz = do
                  putStrLn "ERRO: Número de linhas ou número de colunas distintos"
                  return ERRO  

eOperadorLogico :: String -> Bool
eOperadorLogico "" = False
eOperadorLogico s = if (s=="||" || s=="&&" || s =="<=" || s==">="|| s== "==" || s=="!=" || s == ">" || s == "<" || s== "!") then True
              else False

eOperadorAritmetico :: String -> Bool
eOperadorAritmetico "" = False
eOperadorAritmetico s = if(s=="+" || s=="-" || s=="*" ) then True
                        else False