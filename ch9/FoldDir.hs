module FoldDir where
import System.FilePath((</>), takeFileName, takeExtension)
import Data.Char(toLower)

import ControlledVisit




data Iterate seed = Done      { unwrap :: seed }
                  | Skip      { unwrap :: seed }
                  | Continue  { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
  endSeed <- fold initSeed path
  return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContent subpath >>= walk seed
    
    walk seed (name:names) = do
      let path' = path </> name
      info <- getInfo path'
      case iter seed info of
        done@(Done _) -> return done
        Skip seed'    -> walk seed' names
        Continue seed'
          | isDirectory info -> do
            next <- fold seed' path'
            case next of
              done@(Done _) -> return done
              seed''        -> walk (unwrap seed'') names
          | otherwise -> walk seed' names
    walk seed _ = return (Continue seed)

foldTree' ::([FilePath] -> [FilePath]) -> Iterator a -> a -> FilePath -> IO a
foldTree' order iter initSeed path = do
  endSeed <- fold initSeed path
  return (unwrap endSeed)
  where
    fold seed subpath = do 
      paths <- getUsefulContent subpath
      walk seed $ order paths
    
    walk seed (name:names) = do
      let path' = path </> name
      info <- getInfo path'
      case iter seed info of
        done@(Done _) -> return done
        Skip seed'    -> walk seed' names
        Continue seed'
          | isDirectory info -> do
            next <- fold seed' path'
            case next of
              done@(Done _) -> return done
              seed''        -> walk (unwrap seed'') names
          | otherwise -> walk seed' names
    walk seed _ = return (Continue seed)

atMostThreePictures :: Iterator [FilePath]
atMostThreePictures paths info
  | length paths == 3
    = Done paths
  | ControlledVisit.isDirectory info && takeFileName path == ".svn"
    = Skip paths
  | extension `elem` [".jpg",".png"]
    = Continue (path:paths)
  | otherwise
    = Continue paths
  where extension = map toLower $ takeExtension path
        path = infoPath info

