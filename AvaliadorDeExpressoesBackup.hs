calcularExpressao :: Comandos -> TabelaDeSimbolos -> (Valor,Comandos)
calcularExpressao (cab:cal) tbs = case cab of
                                  "-" -> analisarMenos NULL cal tbs 0 
                                  "!" -> analisarExclamacao NULL cal tbs 0
                                  "(" -> analisarParenteses NULL cal tbs 1
                                  _ -> analisarVariavel NULL cal tbs 0 True
                         

analisarMenos :: Valor -> Comandos -> TabelaDeSimbolos -> Int -> (Valor,Comandos)
analisarMenos valor [] _  qp = (NULL,[])
analisarMenos valor (cab:cal) tbs qp = if(cal == []) then (NULL,cal) 
                                       else if(eOperadorAritmetico cab == True || eOperadorLogico cab == True) then (NULL, cal)
                                       else if(valor == NULL) then case cab of
                                                                   "(" -> analisarParenteses valor cal tbs (qp+1)
                                                                   _ -> analisarVariavel valor cal tbs 0 False 
                                       else
                                       let (cab2:cal2) = cal 
                                       let v = operar "-" cab2 in
                                       if (cal2==[]) then (False, [])
                                       else
                                       let (cab3:cal3) = cal2 then
                                       if()   
