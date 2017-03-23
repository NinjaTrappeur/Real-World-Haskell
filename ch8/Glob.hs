module Glob (
  namesMatching
) where

import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>), isPathSeparator)
import Control.Exception
import Control.Monad (forM)
import GlobRegex (matchesGlob)

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching :: String -> IO [String]
namesMatching pat
  | not (isPattern pat)= do
      exists <- doesNameExist pat
      return [pat | exists]
  | otherwise = do
      case splitFileName pat of
       ("", baseName) -> do
        curDir <- getCurrentDirectory
        listMatches curDir baseName
       (dirName, baseName) -> do
        dirs <- if isPattern dirName
                then namesMatching (dropTrailingPathSeparator dirName)
                else return [dirName]
        let listDir = if isPattern baseName
                      then listMatches
                      else listPlain
        pathNames <- forM dirs $ \dir -> do
                      baseNamesMatches <- listDir dir baseName
                      return (map (dir </>) baseNamesMatches)
        return (concat pathNames)


doesNameExist :: String -> IO Bool
doesNameExist name = do fileExists <- doesFileExist name
                        if fileExists then return True
                                 else doesDirectoryExist name


listMatches :: FilePath -> String -> IO [String]
listMatches dir pat = do
  dirName' <- if null dir
              then getCurrentDirectory
              else return dir
  handle (\(SomeException v) -> (const (return []) v)) $ do
    names <- getDirectoryContents dirName'
    let globMatchingFunc = if isPathSeparator '\\'
                           then matchesGlobCaseSensitive
                           else matchesGlobNotCaseSensitive
    let names' = if isHidden pat
                 then filter isHidden names
                 else filter (not . isHidden) names
    return (filter (globMatchingFunc pat) names')

matchesGlobCaseSensitive :: FilePath -> String -> Bool
matchesGlobCaseSensitive pat dir = matchesGlob dir pat True

matchesGlobNotCaseSensitive :: FilePath -> String -> Bool
matchesGlobNotCaseSensitive pat dir = matchesGlob dir pat False

isHidden :: FilePath -> Bool
isHidden ('.':_) = True
isHidden _ = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <- if null baseName
            then doesDirectoryExist dirName
            else doesNameExist (dirName </> baseName)
  return [baseName | exists]
