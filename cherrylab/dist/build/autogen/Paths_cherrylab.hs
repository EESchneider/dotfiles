module Paths_cherrylab (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/evs/.cabal/bin"
libdir     = "/home/evs/.cabal/lib/x86_64-linux-ghc-7.10.3/cherrylab-0.1.0.0-3IXZeQoXH5XD9Pe4hHQGFe"
datadir    = "/home/evs/.cabal/share/x86_64-linux-ghc-7.10.3/cherrylab-0.1.0.0"
libexecdir = "/home/evs/.cabal/libexec"
sysconfdir = "/home/evs/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cherrylab_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cherrylab_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cherrylab_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cherrylab_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cherrylab_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
