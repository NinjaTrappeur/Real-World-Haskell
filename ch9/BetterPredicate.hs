module BetterPredicate where

import Control.Monad(filterM)
import System.Directory(Permissions(..), getModificationTime, getPermissions)
-- import Data.Time (ClockTime(..))
import Data.Time(UTCTime)
import System.FilePath(takeExtension)
import Control.Exception (bracket, handle, SomeException(SomeException))
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import RecursiveContents (getRecursiveContents)


type Predicate = FilePath
                -> Permissions
                -> Maybe Integer
                -> UTCTime
                -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(SomeException _) ->return Nothing) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
                        file <- openFile path ReadMode
                        size <- hFileSize file
                        hClose file
                        return size

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where check name = do
                      perms <- getPermissions name
                      size <- getFileSize name
                      modified <- getModificationTime name
                      return $ p name perms size modified
  
type InfoP a = FilePath
               -> Permissions
               -> Maybe Integer
               -> UTCTime
               -> a

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

equalP:: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k w x y z = f w x y z == k
-- equalP f k x y z = liftP (==)

liftP:: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

liftP2:: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)

orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
orP = liftP2 (||)

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

(==?)::(Eq a) => InfoP a -> a -> InfoP Bool
(==?) = equalP

(>?), (<?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(>?) = greaterP
(<?) = lesserP

(&&?),(||?):: InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) = andP
(||?) = orP

myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 121072)

-- In order to find the inflix arities, we just mimic those from the default operators
-- gathered using ghci.
