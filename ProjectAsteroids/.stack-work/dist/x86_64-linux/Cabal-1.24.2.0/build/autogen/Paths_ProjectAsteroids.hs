{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ProjectAsteroids (
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

bindir     = "/home/eltusivu/Documents/functionalProgramming_group/ProjectAsteroids/.stack-work/install/x86_64-linux/506122856de0e8c575952690a3ebaa168e97015e4ec95f4c848d6689041da192/8.0.2/bin"
libdir     = "/home/eltusivu/Documents/functionalProgramming_group/ProjectAsteroids/.stack-work/install/x86_64-linux/506122856de0e8c575952690a3ebaa168e97015e4ec95f4c848d6689041da192/8.0.2/lib/x86_64-linux-ghc-8.0.2/ProjectAsteroids-0.1.0.0"
dynlibdir  = "/home/eltusivu/Documents/functionalProgramming_group/ProjectAsteroids/.stack-work/install/x86_64-linux/506122856de0e8c575952690a3ebaa168e97015e4ec95f4c848d6689041da192/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/eltusivu/Documents/functionalProgramming_group/ProjectAsteroids/.stack-work/install/x86_64-linux/506122856de0e8c575952690a3ebaa168e97015e4ec95f4c848d6689041da192/8.0.2/share/x86_64-linux-ghc-8.0.2/ProjectAsteroids-0.1.0.0"
libexecdir = "/home/eltusivu/Documents/functionalProgramming_group/ProjectAsteroids/.stack-work/install/x86_64-linux/506122856de0e8c575952690a3ebaa168e97015e4ec95f4c848d6689041da192/8.0.2/libexec"
sysconfdir = "/home/eltusivu/Documents/functionalProgramming_group/ProjectAsteroids/.stack-work/install/x86_64-linux/506122856de0e8c575952690a3ebaa168e97015e4ec95f4c848d6689041da192/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ProjectAsteroids_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ProjectAsteroids_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ProjectAsteroids_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ProjectAsteroids_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ProjectAsteroids_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ProjectAsteroids_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
