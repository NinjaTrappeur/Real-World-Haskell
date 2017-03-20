import System.Environment (getArgs)
-- Write your own "safe" versions of the standart partial list functions

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit x = Just (end x)
  where end :: [a] -> [a]
        end [a] = []
        end (x:xs) = x:(end xs)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith function list = [(takeWhile function list)] ++ (splitWith function remaining)
    where
        endOfList = dropWhile function list
        remaining
            | null endOfList = []
            | otherwise = tail (endOfList)


-- We can reuse this code which is given in
-- the previous chapter
interactWith function inputFile outputfile = do
    input <- readFile inputFile
    writeFile outputfile (function input)

main = mainWith myFunction
	where myFunction = transposeFile
    	      mainWith function = do
    	      args <- getArgs
    	      case args of
        	      [input,output] -> interactWith function input output
	       	      _ -> putStrLn "error: exactly two arguments needed"


firstWord :: String -> String
firstWord line = head (words line)

firstWordsOfFile :: [String] -> String
firstWordsOfFile [] = ""
firstWordsOfFile (x:xs) = (firstWord x) ++ " " ++ (firstWordsOfFile xs)

getFirstWordsOfFile :: String -> String
getFirstWordsOfFile fileStr = firstWordsOfFile (lines fileStr)