module Paths_lyhgg (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/1002471/.ltshs/2.6/bin"
libdir     = "/Users/1002471/.ltshs/2.6/lib/x86_64-osx-ghc-7.10.2/lyhgg-1.0-ClMEf7G9QiT0lJ6uWAxMPA"
datadir    = "/Users/1002471/.ltshs/2.6/share/x86_64-osx-ghc-7.10.2/lyhgg-1.0"
libexecdir = "/Users/1002471/.ltshs/2.6/libexec"
sysconfdir = "/Users/1002471/.ltshs/2.6/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lyhgg_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lyhgg_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "lyhgg_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lyhgg_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lyhgg_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
