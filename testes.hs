import System.IO
import System.IO.Unsafe

principal :: [Double] -> Bool
principal []  = False
principal (cab:cal) = if (cab/= 2) then False
                      else True

leituraArquivo::Int -> Int -> Int -> [Double]
leituraArquivo 0 _ _ = []
leituraArquivo v l c = unsafePerformIO (intermediario v l c) 
                         
intermediario:: Int -> Int -> Int -> IO[Double]
intermediario 0 _ _ = return []
intermediario v l c = do
                        return (unsafePerformIO(leituraTeclado) : unsafePerformIO (intermediario (v-1) l c))

leituraTeclado::IO(Double)
leituraTeclado = do
                   print "Digite o valor: "
                   numero <- getLine
                   return (read numero::Double)