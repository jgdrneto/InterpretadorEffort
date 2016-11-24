module Interpretador(
interpretar
)where

import Data.List
import TabelaDeSimbolos
import AvaliadorExpressoes
import Tipos
import System.IO.Unsafe

interpretar :: Comandos -> (Bool,Comandos)
interpretar (cab:cal) = if(cab /= "Int") then (False, cal) 
                        else if (cal==[]) then (False, cal) 
                        else 
                        let (cab2:cal2) = cal in
                        if(cab2/= "main") then (False,cal2)
                        else if (cal2==[]) then (False, cal2)
                        else
                        let (cab3:cal3) = cal2 in 
                        if (cab3/="(") then (False, cal3)
                        else if (cal3==[]) then (False,cal3)
                        else
                        let (cab4:cal4) = cal3 in 
                        if (cab4/=")") then (False, cal4)
                        else if (cal4==[]) then (False,cal4)
                        else
                        let (cab5:cal5) = cal4 in 
                        if (cab5/="{") then (False, cal5)
                        else if (cal5==[]) then (False,cal5)
                        else comandos cal5 []  
                              
comandos :: Comandos -> TabelaDeSimbolos ->  (Bool,Comandos)
comandos [] _= (False,[])
comandos (cab:cal) tbs = case cab of
                         "let" -> atribuicao cal tbs
                         "print" -> unsafePerformIO (imprimir cal tbs)

                       --"while" ->	repeticao cal tbs
                       --"if" -> condicional cal tbs
                        --impressao cal tbs
                       --"return" -> return cal tbs
                       --"read" -> leitura cal tbs
                       --
                       --"ggg" -> (False,cal)
--repeticao :: Comandos ->

imprimir :: Comandos -> TabelaDeSimbolos -> IO (Bool,Comandos)
imprimir [] _ = return (False,[])
imprimir cmd tbs = do
                      print tbs
                      return (True, cmd)  

atribuicao :: Comandos -> TabelaDeSimbolos ->(Bool,Comandos)
atribuicao [] _ = (False,[])
atribuicao (nome:cal) tbs = if (cal == []) then (False,cal)
                           else 
                           let (cab2:cal2) = cal in
                           if (cab2/= ":") then (False,cal2)
                           else if (cal2 ==[]) then (False,cal2)
                           else
                           let (tipo:cal3) = cal2 in
                           if (conversorStringParaTipo tipo == SEMTIPO) then (False, cal3)
                           else if(cal3==[]) then (False,cal3) --atribuicaoSemValor nome tipo tbs
                           else
                           let (cab4:cal4) = cal3 in  
                           if (cab4 == ";") then atribuicaoSemValor nome (conversorStringParaTipo tipo) "main" tbs cal4 
                           else if (cal4 == []) then (False,cal4)
                           else if(cab4 /= "=") then (False,cal4)
                           else atribuicaoComValor nome (conversorStringParaTipo tipo) "main" tbs cal4 


atribuicaoSemValor :: Nome -> Tipo -> Escopo -> TabelaDeSimbolos -> Comandos -> (Bool,Comandos)
atribuicaoSemValor nome tipo escopo tbs (cab:cal) = if(cab/= ";") then (False,cal)
                                                    else comandos cal (tbs ++ [criarVariavelSemValor nome tipo escopo])

atribuicaoComValor :: Nome -> Tipo -> Escopo -> TabelaDeSimbolos -> Comandos -> (Bool,Comandos)
atribuicaoComValor "" _ _ _ cs = (False,cs)
atribuicaoComValor _ SEMTIPO _ _ cs = (False,cs)
atribuicaoComValor _ _ "" _ cs = (False,cs)
atribuicaoComValor _ _ _ _ [] = (False,[])
atribuicaoComValor nome tipo escopo tbs cs = case tipo of
                                             Int -> atribuicaoInt nome tipo escopo tbs cs
                                             SEMTIPO -> (False,cs)
                                             Double -> (True,"d":cs)
                                             Float -> atribuicaoFloat nome tipo escopo tbs cs
                                             Char -> (True,"C":cs)
                                             String -> (True,"s":cs)
                                             Polinomio -> (True,"p":cs)
                                             Matriz -> (True,"m":cs)
                                             Bool -> (True,"b":cs)
                                             --Double -> atribuicaoDouble nome tipo escopo tbs cs
                                             --Float -> atribuicaoFloat nome tipo escopo tbs cs

atribuicaoInt :: Nome -> Tipo -> Escopo -> TabelaDeSimbolos -> Comandos -> (Bool,Comandos)
atribuicaoInt "" _ _ _ cs = (False,cs)
atribuicaoInt _ SEMTIPO _ _ cs = (False,cs)
atribuicaoInt _ _ "" _ cs = (False,cs)
atribuicaoInt _ _ _ _ [] = (False,[])
atribuicaoInt nome tipo escopo tbs (cab:cal) = let x = read cab::Int in
                                               let (cab2:cal2) = cal in
                                               if (cab2/=";") then (False,cal2)
                                               else comandos cal2 (tbs ++ [criarVariavelComValor nome tipo (Int_v x) escopo])  

atribuicaoFloat :: Nome -> Tipo -> Escopo -> TabelaDeSimbolos -> Comandos -> (Bool,Comandos)
atribuicaoFloat "" _ _ _ cs = (False,cs)
atribuicaoFloat _ SEMTIPO _ _ cs = (False,cs)
atribuicaoFloat _ _ "" _ cs = (False,cs)
atribuicaoFloat _ _ _ _ [] = (False,[])
atribuicaoFloat nome tipo escopo tbs (cab:cal) = let x = read cab::Float in
                                               let (cab2:cal2) = cal in
                                               if (cab2/=";") then (False,cal2)
                                               else comandos cal2 (tbs ++ [criarVariavelComValor nome tipo (Float_v x) escopo]) 