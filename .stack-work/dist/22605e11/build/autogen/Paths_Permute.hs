{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Permute (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,1,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\trist\\haskell\\Permute\\.stack-work\\install\\8613142c\\bin"
libdir     = "C:\\Users\\trist\\haskell\\Permute\\.stack-work\\install\\8613142c\\lib\\x86_64-windows-ghc-9.4.5\\Permute-0.1.1-CWYfGbHOUPq2FDsUy8lhuz"
dynlibdir  = "C:\\Users\\trist\\haskell\\Permute\\.stack-work\\install\\8613142c\\lib\\x86_64-windows-ghc-9.4.5"
datadir    = "C:\\Users\\trist\\haskell\\Permute\\.stack-work\\install\\8613142c\\share\\x86_64-windows-ghc-9.4.5\\Permute-0.1.1"
libexecdir = "C:\\Users\\trist\\haskell\\Permute\\.stack-work\\install\\8613142c\\libexec\\x86_64-windows-ghc-9.4.5\\Permute-0.1.1"
sysconfdir = "C:\\Users\\trist\\haskell\\Permute\\.stack-work\\install\\8613142c\\etc"

getBinDir     = catchIO (getEnv "Permute_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Permute_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Permute_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Permute_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Permute_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Permute_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
