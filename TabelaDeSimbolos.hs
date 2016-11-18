module TabelaDeSimbolos(

)where

import Data.Array

import Tipos

criarVariavelComValor:: Nome -> Tipo -> Valor -> Escopo -> Simbolo
criarVariavelComValor nome Int valor escopo  =  Simbolo nome Int valor escopo 
criarVariavelComValor nome Float valor escopo  = Simbolo nome Float valor escopo
criarVariavelComValor nome Double valor escopo  = Simbolo nome Double valor escopo
criarVariavelComValor nome Tipos.Char valor escopo  = Simbolo nome Tipos.Char valor escopo
criarVariavelComValor nome Tipos.String valor escopo  = Simbolo nome Tipos.String valor escopo
criarVariavelComValor nome Bool valor escopo  = Simbolo nome Bool valor escopo
criarVariavelComValor nome Polinomio valor escopo  = Simbolo nome Polinomio valor escopo
criarVariavelComValor nome Matriz valor escopo = Simbolo nome Matriz valor escopo

criarVariavelSemValor:: Nome -> Tipo -> Escopo -> Simbolo
criarVariavelSemValor nome Int escopo = Simbolo nome Int NULL escopo 
criarVariavelSemValor nome Float escopo  = Simbolo nome Float NULL escopo 
criarVariavelSemValor nome Double escopo  = Simbolo nome Double NULL escopo 
criarVariavelSemValor nome Tipos.Char escopo  = Simbolo nome Tipos.Char NULL escopo 
criarVariavelSemValor nome Tipos.String escopo  = Simbolo nome Tipos.String NULL escopo 
criarVariavelSemValor nome Bool escopo  = Simbolo nome Bool NULL escopo
criarVariavelSemValor nome Polinomio escopo  = Simbolo nome Polinomio NULL escopo
criarVariavelSemValor nome Matriz escopo = Simbolo nome Matriz NULL escopo

inserirVariavel:: TabelaDeSimbolos -> Simbolo -> TabelaDeSimbolos
inserirVariavel [] s = s:[] 
inserirVariavel tbs simbolo = tbs++(simbolo:[])

buscarValor::TabelaDeSimbolos -> Nome -> Valor
buscarValor [] _ = NULL
buscarValor tbs "" = NULL
buscarValor (cab:cald) n = if(n == nome cab) then valor cab  
                           else buscarValor cald n

buscarTipo::TabelaDeSimbolos -> Nome -> Tipo
buscarTipo [] _ = SEMTIPO
buscarTipo tbs "" = SEMTIPO
buscarTipo (cab:cald) n = if(n == nome cab) then tipo cab  
                          else buscarTipo cald n

buscarEscopo::TabelaDeSimbolos -> Nome -> Escopo
buscarEscopo [] _ = "Nulo"
buscarEscopo tbs "" = "Nulo"
buscarEscopo (cab:cald) n = if(n == nome cab) then escopo cab  
                            else buscarEscopo cald n

excluirPorNome::TabelaDeSimbolos -> Nome -> TabelaDeSimbolos
excluirPorNome [] _ = []
excluirPorNome tab "" = tab
excluirPorNome (cab:cal) n = if (n == nome cab) then cal
                              else cab:excluirPorNome cal n

existeVariavel::TabelaDeSimbolos -> Nome -> Bool
existeVariavel [] _ = False
existeVariavel tab "" = False
existeVariavel (cab:cal) n = if (n == nome cab) then True
                             else existeVariavel cal n

valoresCompativeis::TabelaDeSimbolos -> Nome -> Nome -> Bool
valoresCompativeis [] _ _ = False
valoresCompativeis tab "" _ = False
valoresCompativeis tab _ "" = False
valoresCompativeis tab n1 n2 = buscarTipo tab n1 == buscarTipo tab n2

excluirPorEscopo::TabelaDeSimbolos -> Escopo -> TabelaDeSimbolos
excluirPorEscopo [] _ = []
excluirPorEscopo tab "" = tab
excluirPorEscopo (cab:cal) e = if(e==escopo cab) then excluirPorEscopo cal e
                            else cab:excluirPorEscopo cal e 