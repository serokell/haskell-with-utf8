-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

#include <HsBaseConfig.h>

module Main (main) where

import Prelude hiding (print, putStr, putStrLn)

import Control.Exception.Safe (handleIO, tryIO)
import Control.Monad (filterM, forM_)
import Data.List (sort)
import Data.Version (showVersion)
import Foreign.C.String (CString, peekCAString)
import GHC.IO.Encoding (getLocaleEncoding, initLocaleEncoding)
import GHC.IO.Encoding.Iconv (localeEncodingName)
import GHC.Show (showLitString)
import System.Directory (doesDirectoryExist, doesPathExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Info (arch, compilerName, compilerVersion, os)
import System.IO (hGetEncoding, stderr, stdout)
import System.Process (readProcess)

import qualified Prelude as P


-- | Encode a 'String' to be safe to print in ASCII-only.
protect :: String -> String
protect s = showLitString s ""


putStr :: String -> IO ()
putStr = P.putStr . protect

putStrLn :: String -> IO ()
putStrLn = P.putStrLn . protect

showEnvVar :: String -> IO ()
showEnvVar name = do
  putStr $ "  * " <> name <> " "
  lookupEnv name >>= \case
    Nothing -> putStrLn "is not set"
    Just v -> putStrLn $ "= " <> v


showSystem :: IO ()
showSystem = do
    putStrLn "# System"
    putStrLn $ "  * OS = " <> os
    putStrLn $ "  * arch = " <> arch
    putStrLn $ "  * compiler = "
            <> compilerName <> " " <> showVersion compilerVersion
    showEnvVar "TERM"

showGhc :: IO ()
showGhc = do
    putStrLn "# GHC"
    putStrLn $ "  * initLocaleEncoding = " <> show initLocaleEncoding
    getLocaleEncoding >>= \e -> putStrLn $ "  * locale encoding = " <> show e
    hGetEncoding stdout >>= \e -> putStrLn $ "  * stdout = " <> show e
    hGetEncoding stderr >>= \e -> putStrLn $ "  * stderr = " <> show e

showCbits :: IO ()
showCbits = do
    putStrLn "# C bits"
    putStrLn $ "  * localeEncodingName = " <> localeEncodingName
    showLibcharset
    showLanginfoh
  where
    showLibcharset :: IO ()
    showLibcharset = do
#if defined(HAVE_LIBCHARSET)
      enc <- c_libcharsetEncoding >>= peekCAString
      putStrLn $ "  * libcharset:locale_charset = " <> enc
#else
      putStrLn $ "  * No libcharset."
#endif

    showLanginfoh :: IO ()
    showLanginfoh = do
#if defined(HAVE_LANGINFO_H)
      enc <- c_langinfoEncoding >>= peekCAString
      putStrLn $ "  * langinfo.h:nl_langinfo(CODESET) = " <> enc
#else
      putStrLn $ "  * No <langinfo.h>."
#endif

#if defined(HAVE_LIBCHARSET)
foreign import ccall unsafe "libcharsetEncoding"
  c_libcharsetEncoding :: IO CString
#endif

#if defined(HAVE_LANGINFO_H)
foreign import ccall unsafe "langinfoEncoding"
  c_langinfoEncoding :: IO CString
#endif

showEnv :: IO ()
showEnv = do
    putStrLn "# Environment"
    mapM_ showEnvVar
      [ "LANG"
      , "LC_CTYPE"
      , "LC_ALL="
      ]

showLocales :: IO ()
showLocales = do
    putStrLn "# Locales"
    tryIO callLocalectl >>= \case
      Right out -> do
        putStrLn $ "  * localectl list-locales:"
        showLocaleList (lines out)
      Left _ -> do
        listDir "/usr/lib/locale"
        handleIO (\_ -> pure ()) $ listFile "/usr/lib/locale/locale-archive"
  where
    showLocaleList :: [String] -> IO ()
    showLocaleList locales =
      forM_ (sort locales) $ \item -> putStrLn $ "    * " <> item

    callLocalectl :: IO String
    callLocalectl = readProcess "localectl" ["list-locales"] ""

    listDir :: FilePath -> IO ()
    listDir path = doesPathExist path >>= \case
      False -> putStrLn $ "  * " <> path <> " does not exist."
      True -> doesDirectoryExist path >>= \case
        False -> putStrLn $ "  * " <> path <> " is not a directory."
        True -> do
          putStrLn $ "  * " <> path <> ":"
          ls <- listDirectory path >>= filterM (doesDirectoryExist . (path </>))
          showLocaleList ls

    listFile :: FilePath -> IO ()
    listFile path = doesPathExist path >>= \case
      False -> putStrLn $ "  * " <> path <> " does not exist."
      True -> do
        out <- readProcess "localedef" ["--list", path] ""
        putStrLn $ "  * " <> path <> ":"
        showLocaleList (lines out)



main :: IO ()
main = do
  showSystem
  showGhc
  showCbits
  showEnv
  showLocales
