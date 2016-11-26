module Interpretador(
interpretar,
criarlista,
inserirValores
)where

import Data.List
import TabelaDeSimbolos
import AvaliadorDeExpressoes
import Tipos
import System.IO.Unsafe
import Data.Matrix
import Control.Monad

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
                         "let" -> declaracao cal tbs
                         "print" -> impressao cal tbs 
                         "return" -> retorno cal tbs
                         "read" -> readVar cal tbs
                         _ -> (True,cal)
                       --"while" ->	repeticao cal tbs
                       --"if" -> condicional cal tbs
                        --impressao cal tbs
                       --"return" -> return cal tbs
                       --"read" -> leitura cal tbs
                       --
                       --"ggg" -> (False,cal)
--repeticao :: Comandos ->

readVar :: Comandos -> TabelaDeSimbolos -> (Bool,Comandos)
readVar [] _ = (False,[])
readVar cmds [] = (False, cmds)
readVar (cab:cal) tbs =  if (cab/="(") then (False,cal)
                      else if (cal==[]) then (False,cal)
                      else
                      let (nome:cal2) = cal in
                      let tipo = (buscarTipo tbs nome "main") in
                      if ( tipo == SEMTIPO ) then (False,cal2)
                      else if(cal2==[]) then (False,cal2)
                      else
                      let (cab3:cal3) = cal2 in  
                      if(cab3/=")") then (False,"Erro":cal3)
                      else if(cal3 == []) then (False,"Erro2":cal3)
                      else  
                      let (cab4:cal4) = cal3 in
                      if (cab4 /= ";") then (False,cal4)   --modificado aqui
                      else readEspecifico cal4 tbs tipo nome "main" --unsafePerformIO (printTabela cal4 tbs tipo nome "main")    

printTabela :: Comandos -> TabelaDeSimbolos -> Tipo -> Nome -> Escopo -> IO (Bool,Comandos)
printTabela [] _ _ _ _= return (False, [])
printTabela cmds [] _ _ _ = return (False, cmds)
printTabela cmds tbs Matriz nome escopo = do
                                            print tbs
                                            print nome
                                            print escopo 
                                            return (comandos cmds tbs)

printValores:: Int -> Int -> Int -> TabelaDeSimbolos -> Comandos -> IO (Bool,Comandos)
printValores _ _ _ [] cmds = return (False, cmds) 
printValores l c v tbs cmds = do
                                print l
                                putStrLn "---"
                                print c
                                putStrLn "---"
                                print v
                                putStrLn "---"
                                print tbs
                                putStrLn "---"
                                print cmds
                                return (False, cmds)  
readEspecifico :: Comandos -> TabelaDeSimbolos -> Tipo -> Nome -> Escopo ->(Bool,Comandos)
readEspecifico [] _ _ _ _= (False, [])
readEspecifico cmds [] _ _ _ = (False, cmds)
readEspecifico cmds tbs Matriz nome escopo = definirValores cmds (excluirPorNome tbs nome escopo) Matriz nome escopo 
                                             
                                             --let (qLinhas,qColunas) = unsafePerformIO conseguirValores in
                                             --let valores = definirValores (qLinhas*qColunas) qLinhas qColunas [] in 
                                             --(True,cmds)
                                             --unsafePerformIO ( printValores qLinhas qColunas (qLinhas*qColunas) tbs cmds )
                                             --comandos cmds (ntbs ++ [(criarVariavelComValor nome Matriz (Matriz_v (fromList qLinhas qColunas valores)) escopo)]) 
definirValores :: Comandos -> TabelaDeSimbolos -> Tipo -> Nome -> Escopo ->(Bool,Comandos)
definirValores [] _ _ _ _= (False, [])
definirValores cmds [] _ _ _ = (False, cmds)
definirValores cmds tbs Matriz nome escopo = definirElementos cmds tbs Matriz nome escopo (unsafeDupablePerformIO conseguirValores)

definirElementos:: Comandos -> TabelaDeSimbolos -> Tipo -> Nome -> Escopo -> (Int,Int) ->(Bool,Comandos)
definirElementos [] _ _ _ _ _ = (False, [])
definirElementos cmds [] _ _ _ _= (False, cmds)
definirElementos cmds tbs Matriz nome escopo (lns,cls) = unsafePerformIO( imprimirLinhaColuna cmds tbs Matriz nome escopo lns cls) 
  --comandos cmds (tbs ++ [(criarVariavelComValor nome Matriz (Matriz_v (fromList lns cls (unsafePerformIO(inserirValores (lns*cls) [])))) escopo)])
  --definirConteudo cmds tbs Matriz nome escopo lns cls (unsafePerformIO((inserirValores (criarlista (lns*cls)))))
  --comandos cmds (tbs ++ [(criarVariavelComValor nome Matriz (Matriz_v (fromList lns cls (unsafePerformIO((inserirValores (criarlista (lns*cls) lns cls )))))) escopo)])
  --comandos cmds (tbs ++ [(criarVariavelComValor nome Matriz (Matriz_v (fromList lns cls list)) escopo)])

definirConteudo:: Comandos -> TabelaDeSimbolos  ->Tipo -> Nome -> Escopo -> Int -> Int -> [Double] -> (Bool,Comandos)
definirConteudo [] _ _ _ _ _ _ _ = (False,[])
definirConteudo cmds tbs tipo nome escopo l c lv = comandos cmds (tbs ++ [(criarVariavelComValor nome Matriz (Matriz_v (fromList l c lv)) escopo)])

conseguirValores:: IO (Int,Int)
conseguirValores = do 
                   putStrLn "Digite a quantidade de linhas:"  
                   linhas <- getLine
                   let l = read linhas::Int
                   putStrLn "Digite a quantidade de colunas:"  
                   linhas <- getLine
                   let c = read linhas::Int 
                   return (l,c)

imprimirLinhaColuna::Comandos -> TabelaDeSimbolos -> Tipo -> Nome -> Escopo -> Int -> Int ->IO(Bool,Comandos)
imprimirLinhaColuna cmds tbs tipo nome escopo l c = do
                                                        putStrLn ("Linhas: " ++ show l)
                                                        putStrLn ("Colunas: "++ show c)
                                                        return (unsafePerformIO(imprimirMatriz cmds tbs Matriz nome escopo l c (unsafePerformIO(inserirValores (l*c) [])))) --(definirConteudo cmds tbs tipo nome escopo l c (inserirValores (l*c) []))

imprimirMatriz:: Comandos -> TabelaDeSimbolos -> Tipo -> Nome -> Escopo -> Int -> Int -> [Double] ->IO(Bool,Comandos)
imprimirMatriz cmds tbs tipo nome escopo l c lv = do  
                                                      putStrLn ""
                                                      print lv
                                                      print "Matriz"
                                                      print (fromList l c lv)
                                                      return (definirConteudo cmds tbs Matriz nome escopo l c lv)
criarlista :: Int -> [Double]
criarlista 0  = []
criarlista v = 0.0 : criarlista (v-2)

inserirValores::Int -> [Double] -> IO[Double]
inserirValores v d =  do
                        if (v == length d) then return d
                        else do
                               valor <- definirValorMatriz
                               return (unsafePerformIO(inserirValores v (d++[valor])))

imp:: Matrix Double -> Int -> Int -> IO[Double]
imp v l c = do
              print v
              print l
              print c
              return [] 

definirLinhaColunaMatriz:: Int -> Int -> Int -> Double
definirLinhaColunaMatriz _ 0 _ = 0.0
definirLinhaColunaMatriz _ _ 0 = 0.0
definirLinhaColunaMatriz v l c = let ln = ((modulo (l*c - v) l 0) + 1) in
                                 let cl = ((modulo (l*c - v) c 0) + 1) in
                                 if (cl==0 || ln==0) then 0.0
                                 else unsafePerformIO (definirValorMatriz2 ln cl)

definirValorMatriz::IO(Double)
definirValorMatriz = do
                       putStrLn "Digite o valor: "
                       numero <- getLine
                       let v = read numero::Double
                       return v 

modulo:: Int -> Int -> Int -> Int 
modulo n1 n2 r = if(n1<n2) then r
                 else modulo (n1-n2) n2 (r+1) 

definirValorMatriz2::Int -> Int -> IO(Double)
definirValorMatriz2 l c = do
                           putStrLn ("Digite o valor para linha " ++ (show l) ++ " e coluna " ++ (show c) ++ " :")
                           numero <- getLine
                           return (read numero::Double) 

retorno :: Comandos -> TabelaDeSimbolos -> (Bool,Comandos)
retorno [] _ = (False,[])
retorno (cab:cal) tbs = if(cab/="0") then (False,cal)
                        else if (cal==[]) then (False,cal)
                        else
                        let (cab2:cal2) = cal in
                        if(cab2/=";") then (False,cal2)
                        else if (cal2==[]) then (False,cal2)
                        else
                        let (cab3:cal3) = cal2 in
                        if(cab3/="}") then (False,cal3)
                        else (True, cal3)

impressao :: Comandos -> TabelaDeSimbolos -> (Bool,Comandos)
impressao [] _  = (False,[])
impressao (cab:cal) tbs = if(cab /= "(") then (False, cal)
                          else
                          let (v,cal2) = obterValor cal tbs in
                          let imprimiu = unsafePerformIO (imprimir v) in
                          if(imprimiu==False) then (False,cal2)
                          else if(cal2==[]) then (False,cal2)
                          else
                          let (cab3:cal3) = cal2 in
                          if(cab3/=";") then (False,cal3)
                          else if(cal3==[]) then (False,cal3)
                          else comandos cal3 tbs  

imprimir :: Valor -> IO Bool
imprimir ERRO = return False
imprimir (Int_v i) = do
                       print i
                       return True  
imprimir (Float_v f) = do
                         print f
                         return True
imprimir (Matriz_v m) = do
                         print m
                         return True                         
imprimir _ = return False
declaracao :: Comandos -> TabelaDeSimbolos ->(Bool,Comandos)
declaracao [] _ = (False,[])
declaracao (nome:cal) tbs = if (cal == []) then (False,cal)
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
                           if (cab4 == ";") then declaracaoSemValor nome (conversorStringParaTipo tipo) "main" tbs cal4 
                           else if (cal4 == []) then (False,cal4)
                           else if(cab4 /= "=") then (False,cal4)
                           else declaracaoComValor nome (conversorStringParaTipo tipo) "main" tbs cal4 


declaracaoSemValor :: Nome -> Tipo -> Escopo -> TabelaDeSimbolos -> Comandos -> (Bool,Comandos)
declaracaoSemValor nome tipo escopo tbs (cab:cal) = comandos (cab:cal) (tbs ++ [criarVariavelSemValor nome tipo escopo])

declaracaoComValor :: Nome -> Tipo -> Escopo -> TabelaDeSimbolos -> Comandos -> (Bool,Comandos)
declaracaoComValor "" _ _ _ cs = (False,cs)
declaracaoComValor _ SEMTIPO _ _ cs = (False,cs)
declaracaoComValor _ _ "" _ cs = (False,cs)
declaracaoComValor _ _ _ _ [] = (False,[])
declaracaoComValor nome tipo escopo tbs cs = case tipo of
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