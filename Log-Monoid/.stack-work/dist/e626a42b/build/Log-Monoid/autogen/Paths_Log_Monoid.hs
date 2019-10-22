{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Log_Monoid (
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

bindir     = "C:\\Users\\annel\\Haskell_projects\\Log-Monoid\\.stack-work\\install\\d8956f48\\bin"
libdir     = "C:\\Users\\annel\\Haskell_projects\\Log-Monoid\\.stack-work\\install\\d8956f48\\lib\\x86_64-windows-ghc-8.6.5\\Log-Monoid-0.1.0.0-9qeAQ322NA94x4yCTaQsXx-Log-Monoid"
dynlibdir  = "C:\\Users\\annel\\Haskell_projects\\Log-Monoid\\.stack-work\\install\\d8956f48\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\Users\\annel\\Haskell_projects\\Log-Monoid\\.stack-work\\install\\d8956f48\\share\\x86_64-windows-ghc-8.6.5\\Log-Monoid-0.1.0.0"
libexecdir = "C:\\Users\\annel\\Haskell_projects\\Log-Monoid\\.stack-work\\install\\d8956f48\\libexec\\x86_64-windows-ghc-8.6.5\\Log-Monoid-0.1.0.0"
sysconfdir = "C:\\Users\\annel\\Haskell_projects\\Log-Monoid\\.stack-work\\install\\d8956f48\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Log_Monoid_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Log_Monoid_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Log_Monoid_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Log_Monoid_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Log_Monoid_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Log_Monoid_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
