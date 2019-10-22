{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Asteroids (
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

bindir     = "C:\\Users\\annel\\Haskell_projects\\Asteroids\\.stack-work\\install\\b30b4cfa\\bin"
libdir     = "C:\\Users\\annel\\Haskell_projects\\Asteroids\\.stack-work\\install\\b30b4cfa\\lib\\x86_64-windows-ghc-8.6.5\\Asteroids-0.1.0.0-3jZ5yVbc1r9KxGUtlLtJ2I-Asteroids"
dynlibdir  = "C:\\Users\\annel\\Haskell_projects\\Asteroids\\.stack-work\\install\\b30b4cfa\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\Users\\annel\\Haskell_projects\\Asteroids\\.stack-work\\install\\b30b4cfa\\share\\x86_64-windows-ghc-8.6.5\\Asteroids-0.1.0.0"
libexecdir = "C:\\Users\\annel\\Haskell_projects\\Asteroids\\.stack-work\\install\\b30b4cfa\\libexec\\x86_64-windows-ghc-8.6.5\\Asteroids-0.1.0.0"
sysconfdir = "C:\\Users\\annel\\Haskell_projects\\Asteroids\\.stack-work\\install\\b30b4cfa\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Asteroids_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Asteroids_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Asteroids_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Asteroids_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Asteroids_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Asteroids_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
