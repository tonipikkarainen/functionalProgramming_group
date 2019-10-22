{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_exTyClass1 (
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

bindir     = "C:\\Users\\annel\\Haskell_projects\\exTyClass1\\.stack-work\\install\\abef3a13\\bin"
libdir     = "C:\\Users\\annel\\Haskell_projects\\exTyClass1\\.stack-work\\install\\abef3a13\\lib\\x86_64-windows-ghc-8.6.5\\exTyClass1-0.1.0.0-DVbU9CT8KP5BvEvJLOvPaW-exTyClass1"
dynlibdir  = "C:\\Users\\annel\\Haskell_projects\\exTyClass1\\.stack-work\\install\\abef3a13\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\Users\\annel\\Haskell_projects\\exTyClass1\\.stack-work\\install\\abef3a13\\share\\x86_64-windows-ghc-8.6.5\\exTyClass1-0.1.0.0"
libexecdir = "C:\\Users\\annel\\Haskell_projects\\exTyClass1\\.stack-work\\install\\abef3a13\\libexec\\x86_64-windows-ghc-8.6.5\\exTyClass1-0.1.0.0"
sysconfdir = "C:\\Users\\annel\\Haskell_projects\\exTyClass1\\.stack-work\\install\\abef3a13\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "exTyClass1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "exTyClass1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "exTyClass1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "exTyClass1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "exTyClass1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "exTyClass1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
