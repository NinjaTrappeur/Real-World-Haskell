module Prettify where
import SimpleJSON(JValue(..))
data Doc = Empty
  | Char Char
  | Text String
  | Line
  | Concat Doc Doc
  | Union Doc Doc
    deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text t = Text t

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
a <> Empty = a
Empty <> b = b
a <> b = Concat a b

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate _ [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = x
flatten other = other

compact :: Doc -> String
compact x = transform [x]
  where transform [] = ""
        transform (x:xs) =
          case x of
                    Empty        -> transform xs
                    Char c       -> c:transform xs
                    Text t       -> t ++ transform xs
                    Line         -> '\n':transform xs
                    a `Concat` b -> transform (a:b:xs)
                    _ `Union` b  -> transform (b:xs)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where best col (x:xs) =
          case x of
              Empty        -> best col xs
              Char c       -> c:best (col + 1) xs
              Text t       -> t ++ best (col + length xs) xs
              Line         -> '\n' : best 0 xs
              a `Concat` b -> best col (a:b:xs)
              a `Union` b  -> nicest col (best col (a:xs)) (best col (b:xs))
        best _ _ = ""
        nicest col a b | (width - least) `fits` a = a
                       | otherwise                = b
              where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` "" = True
w `fits` ('\n':_) = True
w `fits` (c:cs) = fits (w - 1) cs
