{- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Main
  ( main
  ) where

import Control.Monad (unless)
import GHC.IO.Encoding (mkTextEncoding, setLocaleEncoding)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Test.Tasty (defaultMain)

import qualified System.IO as IO

import System.IO.Utf8 (withUtf8StdHandles)

import Tree (tests)

main :: IO ()
main = withUtf8StdHandles $ do
  stdoutIsTerm <- IO.hIsTerminalDevice IO.stdout
  unless stdoutIsTerm $ do
    putStrLn "Some tests assume that stdout is a terminal device"
    exitWith (ExitFailure 1)

  -- The issue only shows up when current locale encoding is ASCII.
  -- Realistically, very often when running this test this will not be
  -- the case, so we unset locale encoding manually.
  mkTextEncoding "ASCII" >>= setLocaleEncoding
  tests >>= defaultMain
