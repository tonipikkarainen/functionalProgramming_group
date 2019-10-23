{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_IOoperations (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\annel\\Haskell_projects\\functionalProgramming_group\\IOoperations\\.stack-work\\install\\39f7df72\\bin"
libdir     = "C:\\Users\\annel\\Haskell_projects\\functionalProgramming_group\\IOoperations\\.stack-work\\install\\39f7df72\\lib\\x86_64-windows-ghc-8.6.5\\IOoperations-0.1.0.0-IYtK2qz3nHv5Ogdp01ooJA-IOoperations"
dynlibdir  = "C:\\Users\\annel\\Haskell_projects\\functionalProgramming_group\\IOoperations\\.stack-work\\install\\39f7df72\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\Users\\annel\\Haskell_projects\\functionalProgramming_group\\IOoperations\\.stack-work\\install\\39f7df72\\share\\x86_64-windows-ghc-8.6.5\\IOoperations-0.1.0.0"
libexecdir = "C:\\Users\\annel\\Haskell_projects\\functionalProgramming_group\\IOoperations\\.stack-work\\install\\39f7df72\\libexec\\x86_64-windows-ghc-8.6.5\\IOoperations-0.1.0.0"
sysconfdir = "C:\\Users\\annel\\Haskell_projects\\functionalProgramming_group\\IOoperations\\.stack-work\\install\\39f7df72\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "IOoperations_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "IOoperations_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "IOoperations_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "IOoperations_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "IOoperations_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "IOoperations_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
