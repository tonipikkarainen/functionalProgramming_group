{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_testquick (
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

bindir     = "/Users/tonipikkarainen/master_degree/tiea341/group/testquick/.stack-work/install/x86_64-osx/cb47f89ad773e48407fb8482bc1766ddb9c56b90ae591fdeb1efd98faed6a3d5/8.6.5/bin"
libdir     = "/Users/tonipikkarainen/master_degree/tiea341/group/testquick/.stack-work/install/x86_64-osx/cb47f89ad773e48407fb8482bc1766ddb9c56b90ae591fdeb1efd98faed6a3d5/8.6.5/lib/x86_64-osx-ghc-8.6.5/testquick-0.1.0.0-8r2AqvKyUbp2jEDrsE1JP8-testquick"
dynlibdir  = "/Users/tonipikkarainen/master_degree/tiea341/group/testquick/.stack-work/install/x86_64-osx/cb47f89ad773e48407fb8482bc1766ddb9c56b90ae591fdeb1efd98faed6a3d5/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/tonipikkarainen/master_degree/tiea341/group/testquick/.stack-work/install/x86_64-osx/cb47f89ad773e48407fb8482bc1766ddb9c56b90ae591fdeb1efd98faed6a3d5/8.6.5/share/x86_64-osx-ghc-8.6.5/testquick-0.1.0.0"
libexecdir = "/Users/tonipikkarainen/master_degree/tiea341/group/testquick/.stack-work/install/x86_64-osx/cb47f89ad773e48407fb8482bc1766ddb9c56b90ae591fdeb1efd98faed6a3d5/8.6.5/libexec/x86_64-osx-ghc-8.6.5/testquick-0.1.0.0"
sysconfdir = "/Users/tonipikkarainen/master_degree/tiea341/group/testquick/.stack-work/install/x86_64-osx/cb47f89ad773e48407fb8482bc1766ddb9c56b90ae591fdeb1efd98faed6a3d5/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "testquick_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "testquick_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "testquick_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "testquick_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "testquick_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "testquick_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
