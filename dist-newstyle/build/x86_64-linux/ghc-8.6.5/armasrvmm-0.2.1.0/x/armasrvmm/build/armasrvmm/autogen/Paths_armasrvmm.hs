{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_armasrvmm (
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
version = Version [0,2,1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/blackraider/.cabal/bin"
libdir     = "/home/blackraider/.cabal/lib/x86_64-linux-ghc-8.6.5/armasrvmm-0.2.1.0-inplace-armasrvmm"
dynlibdir  = "/home/blackraider/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/blackraider/.cabal/share/x86_64-linux-ghc-8.6.5/armasrvmm-0.2.1.0"
libexecdir = "/home/blackraider/.cabal/libexec/x86_64-linux-ghc-8.6.5/armasrvmm-0.2.1.0"
sysconfdir = "/home/blackraider/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "armasrvmm_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "armasrvmm_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "armasrvmm_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "armasrvmm_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "armasrvmm_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "armasrvmm_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
