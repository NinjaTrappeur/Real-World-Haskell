import System.IO
import Control.Monad(unless)
import Data.Char(toUpper)

main :: IO()
main = do
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       mainloop inh outh
       hClose inh
       hClose outh


mainloop :: Handle -> Handle -> IO()
mainloop inh outh=  do
                    iEof <- hIsEOF inh
                    unless iEof $
                      do str <- hGetLine inh
                         hPutStrLn outh (map toUpper str)
                         mainloop inh outh
