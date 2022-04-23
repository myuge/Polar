{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_polar (
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
version = Version [0,0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/myuge/.cabal/bin"
libdir     = "/home/myuge/.cabal/lib/x86_64-freebsd-ghc-8.8.3/polar-0.0.0.0-inplace-polar-benchmark"
dynlibdir  = "/home/myuge/.cabal/lib/x86_64-freebsd-ghc-8.8.3"
datadir    = "/home/myuge/.cabal/share/x86_64-freebsd-ghc-8.8.3/polar-0.0.0.0"
libexecdir = "/home/myuge/.cabal/libexec/x86_64-freebsd-ghc-8.8.3/polar-0.0.0.0"
sysconfdir = "/home/myuge/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "polar_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "polar_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "polar_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "polar_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "polar_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "polar_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
