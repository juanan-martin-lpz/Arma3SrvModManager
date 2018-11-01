{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_12bdi_launcher (
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

bindir     = "C:\\Users\\the_b\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\the_b\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.3\\12bdi-launcher-0.1.0.0-9RAQvyRIOkJBwXTjOvgTZl-12bdi-launcher"
dynlibdir  = "C:\\Users\\the_b\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.3"
datadir    = "C:\\Users\\the_b\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.3\\12bdi-launcher-0.1.0.0"
libexecdir = "C:\\Users\\the_b\\AppData\\Roaming\\cabal\\12bdi-launcher-0.1.0.0-9RAQvyRIOkJBwXTjOvgTZl-12bdi-launcher\\x86_64-windows-ghc-8.4.3\\12bdi-launcher-0.1.0.0"
sysconfdir = "C:\\Users\\the_b\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "12bdi_launcher_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "12bdi_launcher_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "12bdi_launcher_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "12bdi_launcher_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "12bdi_launcher_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "12bdi_launcher_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
