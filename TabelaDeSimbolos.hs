module TabelaDeSimbolos(

)where

import Tipos

criarSimbolo:: Tipos.Nome -> [Valor] -> Tipo -> Escopo -> Simbolo
criarSimbolo (nome,valor,tipo,escopo) = (nome,valor,tipo,escopo)

inserirSimbolo:: TabelaDeSimbolos -> Simbolo -> TabelaDeSimbolos
inserirSimbolo ([],s) = s:[]
inserirSimbolo (tbs,()) = tbs 
inserirSimbolo (tbs,simbolo) = if (procurarSimbolo tbs simbolo == ()) then tbs:simbolo
							   else tbs	

excluirSimbolo:: TabelaDeSimbolos -> Simbolo -> TabelaDeSimbolos
excluirSimbolo([],simbolo) = []
excluirSimbolo(tbs,()) = tbs
excluirSimbolo(cbtbs:caldatbs,simbolo) = if(fst simbolo == fst cbtbs) then caldatbs
										 else  cbtbs:excluirSimbolo caldatbs simbolo

procurarSimbolo:: TabelaDeSimbolos -> Simbolo -> Simbolo
procurarSimbolo([],()) = ()
procurarSimbolo([], s) = ()
procurarSimbolo(tbs,()) = ()
procurarSimbolo(cbtbs:caldatbs,simbolo) = if(fst simbolo == fst cbtbs) then cbtbs
										  else  cbtbs:procurarSimbolo caldatbs simbolo

existeSimbolo:: TabelaDeSimbolos -> Simbolo -> Bool
existeSimbolo([], s) = False
existeSimbolo(tbs,()) = False
existeSimbolo(cbtbs:caldatbs,simbolo) = if(fst simbolo == fst cbtbs) then True
										else existeSimbolo caldatbs simbolo

procurarSimboloPorNome:: TabelaDeSimbolos -> Nome -> Simbolo
procurarSimboloPorNome([],"") = ()
procurarSimboloPorNome(tbs,"") = ()
procurarSimboloPorNome([],s) = ()
procurarSimboloPorNome(cbtbs:caldatbs,simbolo) = if(fst simbolo == fst cbtbs) then cbtbs
												 else procurarSimboloPorNome caldatbs simbolo

procurarValorPorNome:: TabelaDeSimbolos -> Nome -> Valor 
procurarValorPorNome([],nome) = []
procurarValorPorNome(tbs,"") = []
procurarValorPorNome(tbs, nome) = procurarSimboloPorNome(tbs,nome)


procurarValorNoSimbolo:: Simbolo -> Nome -> Valor
procurarValorNoSimbolo((),"") = NULL