{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_extyjson (
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

bindir     = "/Users/tonipikkarainen/master_degree/tiea341/group/credit4/extyjson/.stack-work/install/x86_64-osx/2944a8edff260373339c8b16e54dc111fb259580f9f40c7e743cdf60d6434bf1/8.6.5/bin"
libdir     = "/Users/tonipikkarainen/master_degree/tiea341/group/credit4/extyjson/.stack-work/install/x86_64-osx/2944a8edff260373339c8b16e54dc111fb259580f9f40c7e743cdf60d6434bf1/8.6.5/lib/x86_64-osx-ghc-8.6.5/extyjson-0.1.0.0-74QRAfrkQAN8MeV2MqhMyF-extyjson"
dynlibdir  = "/Users/tonipikkarainen/master_degree/tiea341/group/credit4/extyjson/.stack-work/install/x86_64-osx/2944a8edff260373339c8b16e54dc111fb259580f9f40c7e743cdf60d6434bf1/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/tonipikkarainen/master_degree/tiea341/group/credit4/extyjson/.stack-work/install/x86_64-osx/2944a8edff260373339c8b16e54dc111fb259580f9f40c7e743cdf60d6434bf1/8.6.5/share/x86_64-osx-ghc-8.6.5/extyjson-0.1.0.0"
libexecdir = "/Users/tonipikkarainen/master_degree/tiea341/group/credit4/extyjson/.stack-work/install/x86_64-osx/2944a8edff260373339c8b16e54dc111fb259580f9f40c7e743cdf60d6434bf1/8.6.5/libexec/x86_64-osx-ghc-8.6.5/extyjson-0.1.0.0"
sysconfdir = "/Users/tonipikkarainen/master_degree/tiea341/group/credit4/extyjson/.stack-work/install/x86_64-osx/2944a8edff260373339c8b16e54dc111fb259580f9f40c7e743cdf60d6434bf1/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "extyjson_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "extyjson_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "extyjson_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "extyjson_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "extyjson_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "extyjson_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
