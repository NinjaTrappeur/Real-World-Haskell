module ControllerVisit where

import System.IO(openFile, IOMode(..), hClose, hFileSize)
import System.Directory(Permissions, searchable, getDirectoryContents, getPermissions, getModificationTime)
import System.FilePath((</>))
import Control.Monad (forM, liftM, filterM)
import Control.Exception(handle, SomeException(SomeException), bracket)
import Data.Time(UTCTime)

data Info = Info {
    infoPath :: FilePath
  , infoPerms :: Maybe Permissions
  , infoSize :: Maybe Integer
  , infoModTime :: Maybe UTCTime
} deriving (Eq, Ord, Show)

type InfoP a = Info -> a

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\(SomeException _)-> return Nothing) (Just `liftM` act)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContent path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents) $ \info -> 
    if isDirectory info && (infoPath info /= path)
      then ControllerVisit.traverse order (infoPath info)
      else return [info]

getUsefulContent :: FilePath -> IO [String]
getUsefulContent path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".",".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

postOrder :: [Info] -> [Info]
postOrder (x:xs) = if isDirectory x
  then postOrder xs ++ [x]
  else x:postOrder xs
postOrder [] = []

-- Predicates/Combinators
liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP op infoP test info = infoP info `op` test

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

equalsP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalsP = liftP (==)

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 op pred1 pred2 info = pred1 info `op` pred2 info

andP, orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)
orP = liftP2 (||)

(==?)::(Eq a) => InfoP a -> a -> InfoP Bool
(==?) = equalsP

(>?), (<?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(>?) = greaterP
(<?) = lesserP

(&&?),(||?):: InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) = andP
(||?) = orP

traverse' :: ([Info] -> [Info]) -> InfoP Bool -> FilePath -> IO [Info]
traverse' order pred path = ControllerVisit.traverse (order . filter pred) path

