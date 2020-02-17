{- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Main
  ( main
  ) where

import GHC.IO.Encoding (mkTextEncoding, setLocaleEncoding)
import Main.Utf8 (withUtf8)
import Test.Tasty (defaultMain)

import Tree (tests)

main :: IO ()
main = withUtf8 $ do
  -- The issue only shows up when current locale encoding is ASCII.
  -- Realistically, very often when running this test this will not be
  -- the case, so we unset locale encoding manually.
  mkTextEncoding "ASCII" >>= setLocaleEncoding
  tests >>= defaultMain
