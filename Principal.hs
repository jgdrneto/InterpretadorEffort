import System.IO
import System.IO.Unsafe

import AnalisadorLexico
import AnalisadorSintatico
import Interpretador

principal :: String -> [String]
principal fn =  interpretar(analisadorSintatico(analisadorLexico(unsafePerformIO (leia fn))))

leia fn = do x <- openFile fn ReadMode
             y <- hGetContents x
             return (words y)
