import Data.Char (digitToInt)

asInt_fold :: String -> Int
asInt_fold [] = error "Cannot convert [] to Int"
asInt_fold (x:xs)
    | x == '-' && null xs  = error "Cannot convert this expression to Int."
    | x == '-' = (-1) * asInt_fold xs
asInt_fold str = foldl foldFunc 0 str
    where foldFunc :: Int -> Char -> Int
          foldFunc acc elem = (digitToInt elem) + (acc * 10)

type ErrorMessage = String

asInt_either :: String -> Either ErrorMessage Int
asInt_either [] = Left "Cannot convert [] to Int"
asInt_either (x:xs)
    | x == '-' && null xs  = Left "Cannot convert this expression to Int."
    | x == '-' = fmap (*(-1)) (asInt_either xs)
asInt_either str = Right (foldl foldFunc 0 str)
    where foldFunc :: Int -> Char -> Int
          foldFunc acc elem = (digitToInt elem) + (acc * 10)


concat_foldr :: [[a]] -> [a]
concat_foldr input = foldr foldFunc [] input
    where foldFunc :: [a] -> [a] -> [a]
          foldFunc acc elem = acc ++ elem


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' pred (x:xs)
    | pred x = x:takeWhile' pred xs
    | otherwise = []

takeWhileFold :: (a -> Bool) -> [a] -> [a]
takeWhileFold pred input = foldr foldFunc [] input
    where foldFunc elem acc
            | pred elem = elem:acc
            | otherwise = []

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' pred input = foldl foldFunc [] input
    where foldFunc [] elem = [[elem]]
          foldFunc acc elem
            | pred (head(last acc)) elem = (init acc) ++ [(last acc ++ [elem])]
            | otherwise = acc ++ [[elem]]




