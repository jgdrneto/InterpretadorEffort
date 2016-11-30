module AnalisadorLexico(
analisadorLexico
)where

import Data.List.Split
import Data.List

--Descrição: Quebra todas as palavras que tenham o operador "!="
--Entrada: String
--Saída: Lista com os termos da string quebrada
anaLex2Op1:: String  -> [String]
anaLex2Op1 s = split (onSublist "!=") s


--Descrição: Quebra todas as palavras que tenham o operador "=="
--Entrada: Lista com todas as strings
--Saída: Lista com os termos da string quebrada
anaLex2Op2:: [String] -> [String]
anaLex2Op2 [] = []
anaLex2Op2 (cab:cal) = split (onSublist "==") cab ++ anaLex2Op2 cal


--Descrição: Quebra todas as palavras que tenham o operador "||"
--Entrada: Lista com todas as strings
--Saída: Lista com os termos da string quebrada
anaLex2Op3:: [String] -> [String]
anaLex2Op3 [] = []
anaLex2Op3 (cab:cal) = split (onSublist "||") cab ++ anaLex2Op3 cal


--Descrição: Quebra todas as palavras que tenham o operador "&&"
--Entrada: Lista com todas as strings
--Saída: Lista com os termos da string quebrada
anaLex2Op4:: [String] -> [String]
anaLex2Op4 [] = []
anaLex2Op4 (cab:cal) = split (onSublist "&&") cab ++ anaLex2Op4 cal


--Descrição: Quebra todas as palavras que tenham o operador "<="
--Entrada: Lista com todas as strings
--Saída: Lista com os termos da string quebrada
anaLex2Op5:: [String] -> [String]
anaLex2Op5 [] = []
anaLex2Op5 (cab:cal) = split (onSublist "<=") cab ++ anaLex2Op5 cal


--Descrição: Quebra todas as palavras que tenham o operador ">="
--Entrada: Lista com todas as strings
--Saída: Lista com os termos da string quebrada
anaLex2Op6:: [String] -> [String]
anaLex2Op6 [] = []
anaLex2Op6 (cab:cal) = split (onSublist ">=") cab ++ anaLex2Op6 cal


--Descrição: Quebra todas as palavras que tenham o operadores unários suportados 
--Entrada: Lista com todas as strings
--Saída: Lista com os termos da string quebrada
anaLex1Op:: [String] -> [String]
anaLex1Op [] = []
anaLex1Op (cab:cal) = if(isInfixOf "==" cab || isInfixOf "!=" cab ||  isInfixOf ">=" cab ||  isInfixOf "<=" cab) then cab : anaLex1Op cal 
                        else split (oneOf "<>()[]{}-+*=:,;!") cab ++ anaLex1Op cal

--Descrição: Retirar as String vazias
--Entrada: Lista com algumas Strings vazias
--Saída:  Lista sem Strings quebradas
retirarStringVazia::[String] -> [String]
retirarStringVazia [] = []
retirarStringVazia (cab:cal) = if(cab=="") then retirarStringVazia cal
                                  else cab : retirarStringVazia cal  

--Descição: Analisa a lista de strings do programa
--Entrada: Lista com as palavras
--Saida: Lista com os lexemas
analisadorLexico:: [String] -> [String]
analisadorLexico [] = []
analisadorLexico (cab:cal) = retirarStringVazia(anaLex1Op (anaLex2Op6 (anaLex2Op5 (anaLex2Op4 (anaLex2Op3 (anaLex2Op2 (anaLex2Op1 cab))))))) ++ analisadorLexico cal 