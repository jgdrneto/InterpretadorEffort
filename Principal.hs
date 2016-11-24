import System.IO
import System.IO.Unsafe

import AnalisadorLexico
import Interpretador

principal :: String -> (Bool,[String])
principal fn =  interpretar(analisadorLexico(unsafePerformIO (leia fn)))

leia fn = do x <- openFile fn ReadMode
             y <- hGetContents x
             return (words y)
