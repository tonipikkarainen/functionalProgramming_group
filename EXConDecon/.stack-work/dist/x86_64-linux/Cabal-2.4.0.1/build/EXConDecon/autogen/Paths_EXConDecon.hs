{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_EXConDecon (
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

bindir     = "/home/eltusivu/Documents/functionalProgramming_group/EXConDecon/.stack-work/install/x86_64-linux/721c7b038cc166fc2cb31d55c71a682d8a0f5ff54c2aa6089e673fc86d0a3716/8.6.5/bin"
libdir     = "/home/eltusivu/Documents/functionalProgramming_group/EXConDecon/.stack-work/install/x86_64-linux/721c7b038cc166fc2cb31d55c71a682d8a0f5ff54c2aa6089e673fc86d0a3716/8.6.5/lib/x86_64-linux-ghc-8.6.5/EXConDecon-0.1.0.0-I0TRfbzf7FYCEecUgJ3BJ8-EXConDecon"
dynlibdir  = "/home/eltusivu/Documents/functionalProgramming_group/EXConDecon/.stack-work/install/x86_64-linux/721c7b038cc166fc2cb31d55c71a682d8a0f5ff54c2aa6089e673fc86d0a3716/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/eltusivu/Documents/functionalProgramming_group/EXConDecon/.stack-work/install/x86_64-linux/721c7b038cc166fc2cb31d55c71a682d8a0f5ff54c2aa6089e673fc86d0a3716/8.6.5/share/x86_64-linux-ghc-8.6.5/EXConDecon-0.1.0.0"
libexecdir = "/home/eltusivu/Documents/functionalProgramming_group/EXConDecon/.stack-work/install/x86_64-linux/721c7b038cc166fc2cb31d55c71a682d8a0f5ff54c2aa6089e673fc86d0a3716/8.6.5/libexec/x86_64-linux-ghc-8.6.5/EXConDecon-0.1.0.0"
sysconfdir = "/home/eltusivu/Documents/functionalProgramming_group/EXConDecon/.stack-work/install/x86_64-linux/721c7b038cc166fc2cb31d55c71a682d8a0f5ff54c2aa6089e673fc86d0a3716/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "EXConDecon_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "EXConDecon_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "EXConDecon_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "EXConDecon_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "EXConDecon_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "EXConDecon_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
