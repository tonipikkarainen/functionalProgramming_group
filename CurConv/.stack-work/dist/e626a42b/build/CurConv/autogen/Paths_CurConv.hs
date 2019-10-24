{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_CurConv (
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

bindir     = "C:\\Users\\annel\\Haskell_projects\\functionalProgramming_group\\CurConv\\.stack-work\\install\\39f7df72\\bin"
libdir     = "C:\\Users\\annel\\Haskell_projects\\functionalProgramming_group\\CurConv\\.stack-work\\install\\39f7df72\\lib\\x86_64-windows-ghc-8.6.5\\CurConv-0.1.0.0-FFTSJpQvTynHwpUv0bf4t0-CurConv"
dynlibdir  = "C:\\Users\\annel\\Haskell_projects\\functionalProgramming_group\\CurConv\\.stack-work\\install\\39f7df72\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\Users\\annel\\Haskell_projects\\functionalProgramming_group\\CurConv\\.stack-work\\install\\39f7df72\\share\\x86_64-windows-ghc-8.6.5\\CurConv-0.1.0.0"
libexecdir = "C:\\Users\\annel\\Haskell_projects\\functionalProgramming_group\\CurConv\\.stack-work\\install\\39f7df72\\libexec\\x86_64-windows-ghc-8.6.5\\CurConv-0.1.0.0"
sysconfdir = "C:\\Users\\annel\\Haskell_projects\\functionalProgramming_group\\CurConv\\.stack-work\\install\\39f7df72\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "CurConv_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "CurConv_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "CurConv_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "CurConv_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CurConv_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CurConv_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
