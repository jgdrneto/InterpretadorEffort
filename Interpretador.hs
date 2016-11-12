import System.IO
import System.IO.Unsafe

import AnalisadorLexico

principal :: String -> [String]
principal fn =  analisadorLexico(unsafePerformIO (leia fn))

leia fn = do x <- openFile fn ReadMode
             y <- hGetContents x
             return (words y)
