{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_logMonoid (
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

bindir     = "/home/eltusivu/Documents/logMonoid/.stack-work/install/x86_64-linux/ec7b7a032a51bc78aff36a8fc3c204dcedb1f37c2bcd1edcfbd184e9eb2e7c81/8.6.5/bin"
libdir     = "/home/eltusivu/Documents/logMonoid/.stack-work/install/x86_64-linux/ec7b7a032a51bc78aff36a8fc3c204dcedb1f37c2bcd1edcfbd184e9eb2e7c81/8.6.5/lib/x86_64-linux-ghc-8.6.5/logMonoid-0.1.0.0-4kIly9xmZNDbWGxzcNCic-logMonoid"
dynlibdir  = "/home/eltusivu/Documents/logMonoid/.stack-work/install/x86_64-linux/ec7b7a032a51bc78aff36a8fc3c204dcedb1f37c2bcd1edcfbd184e9eb2e7c81/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/eltusivu/Documents/logMonoid/.stack-work/install/x86_64-linux/ec7b7a032a51bc78aff36a8fc3c204dcedb1f37c2bcd1edcfbd184e9eb2e7c81/8.6.5/share/x86_64-linux-ghc-8.6.5/logMonoid-0.1.0.0"
libexecdir = "/home/eltusivu/Documents/logMonoid/.stack-work/install/x86_64-linux/ec7b7a032a51bc78aff36a8fc3c204dcedb1f37c2bcd1edcfbd184e9eb2e7c81/8.6.5/libexec/x86_64-linux-ghc-8.6.5/logMonoid-0.1.0.0"
sysconfdir = "/home/eltusivu/Documents/logMonoid/.stack-work/install/x86_64-linux/ec7b7a032a51bc78aff36a8fc3c204dcedb1f37c2bcd1edcfbd184e9eb2e7c81/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "logMonoid_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "logMonoid_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "logMonoid_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "logMonoid_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "logMonoid_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "logMonoid_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
