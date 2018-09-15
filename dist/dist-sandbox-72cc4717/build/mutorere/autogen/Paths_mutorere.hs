{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_mutorere (
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
version = Version [1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/scott/projects/mutorere/.cabal-sandbox/bin"
libdir     = "/Users/scott/projects/mutorere/.cabal-sandbox/lib/x86_64-osx-ghc-8.4.3/mutorere-1.0.0-2mrdlWirmVg6xjzxEPymfY"
dynlibdir  = "/Users/scott/projects/mutorere/.cabal-sandbox/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/scott/projects/mutorere/.cabal-sandbox/share/x86_64-osx-ghc-8.4.3/mutorere-1.0.0"
libexecdir = "/Users/scott/projects/mutorere/.cabal-sandbox/libexec/x86_64-osx-ghc-8.4.3/mutorere-1.0.0"
sysconfdir = "/Users/scott/projects/mutorere/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mutorere_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mutorere_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "mutorere_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "mutorere_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mutorere_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mutorere_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
