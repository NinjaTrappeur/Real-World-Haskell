module PrettyStub where

import SimpleJSON

data Doc = ToBeDefined deriving (Show)

double :: Double -> Doc
double n = undefined

text :: String -> Doc
text s = undefined

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat docs = undefined


