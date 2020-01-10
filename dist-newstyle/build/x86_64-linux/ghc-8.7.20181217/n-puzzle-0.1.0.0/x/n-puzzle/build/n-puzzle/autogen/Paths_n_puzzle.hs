{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_n_puzzle (
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

bindir     = "/home/lazrossi/.cabal/bin"
libdir     = "/home/lazrossi/.cabal/lib/x86_64-linux-ghc-8.7.20181217/n-puzzle-0.1.0.0-inplace-n-puzzle"
dynlibdir  = "/home/lazrossi/.cabal/lib/x86_64-linux-ghc-8.7.20181217"
datadir    = "/home/lazrossi/.cabal/share/x86_64-linux-ghc-8.7.20181217/n-puzzle-0.1.0.0"
libexecdir = "/home/lazrossi/.cabal/libexec/x86_64-linux-ghc-8.7.20181217/n-puzzle-0.1.0.0"
sysconfdir = "/home/lazrossi/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "n_puzzle_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "n_puzzle_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "n_puzzle_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "n_puzzle_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "n_puzzle_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "n_puzzle_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
