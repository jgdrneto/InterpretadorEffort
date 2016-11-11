import System.IO
import System.IO.Unsafe
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
 
principal :: String -> [String]
principal fn =  analisadorLexico(unsafePerformIO (leia fn))

anaLex2Op1:: String  -> [String]
anaLex2Op1 s = split (onSublist "!=") s

anaLex2Op2:: [String] -> [String]
anaLex2Op2 [] = []
anaLex2Op2 (cab:cal) = split (onSublist "==") cab ++ anaLex2Op2 cal

anaLex2Op3:: [String] -> [String]
anaLex2Op3 [] = []
anaLex2Op3 (cab:cal) = split (onSublist "||") cab ++ anaLex2Op3 cal

anaLex2Op4:: [String] -> [String]
anaLex2Op4 [] = []
anaLex2Op4 (cab:cal) = split (onSublist "&&") cab ++ anaLex2Op4 cal

anaLex1Op:: [String] -> [String]
anaLex1Op [] = []
anaLex1Op (cab:cal) = if(isInfixOf "==" cab || isInfixOf "!=" cab) then cab : anaLex1Op cal 
                        else split (oneOf "()[]{}-+*=:,.;!") cab ++ anaLex1Op cal

retirarStringVazia::[String] -> [String]
retirarStringVazia [] = []
retirarStringVazia (cab:cal) = if(cab=="") then retirarStringVazia cal
                                  else cab : retirarStringVazia cal  

analisadorLexico:: [String] -> [String]
analisadorLexico [] = []
analisadorLexico (cab:cal) = retirarStringVazia(anaLex1Op (anaLex2Op4 (anaLex2Op3 (anaLex2Op2 (anaLex2Op1 cab))))) ++ analisadorLexico cal 

leia fn = do x <- openFile fn ReadMode
             y <- hGetContents x
             return (words y)