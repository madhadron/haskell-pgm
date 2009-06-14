module Paths_pgm (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/ross/.cabal/bin"
libdir     = "/Users/ross/.cabal/lib/pgm-0.1/ghc-6.10.1"
datadir    = "/Users/ross/.cabal/share/pgm-0.1"
libexecdir = "/Users/ross/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "pgm_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "pgm_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "pgm_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "pgm_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
