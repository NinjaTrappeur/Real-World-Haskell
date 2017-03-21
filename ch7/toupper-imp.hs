import System.IO
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
                    isEOF <- hIsEOF inh
                    if isEOF
                      then return ()
                      else do str <- hGetLine inh
                              hPutStrLn outh (map toUpper str)
                              mainloop inh outh
