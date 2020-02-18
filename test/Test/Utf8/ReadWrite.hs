{- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Utf8.ReadWrite where

import Control.DeepSeq (force)
import Control.Exception (IOException, evaluate)
import Control.Exception.Safe (MonadMask, try)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import GHC.IO.Encoding (utf8)
import System.IO (FilePath)
import System.IO.Temp (withSystemTempFile)

import Hedgehog (Property, (===), forAll, property)
import Test.HUnit (Assertion, assertFailure)

import qualified Data.Text.IO as T
import qualified Data.Text.IO.Utf8 as Utf8
import qualified System.IO as IO
import qualified System.IO.Utf8 as Utf8

import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R


-- | Helper that writes Text to a temp file.
withTestFile :: (MonadIO m, MonadMask m) => Text -> (FilePath -> m r) -> m r
withTestFile t act = withSystemTempFile "utf8.txt" $ \fp h -> do
    liftIO $ IO.hSetEncoding h utf8 *> T.hPutStr h t *> IO.hClose h
    act fp


-- | Sanity check.
unit_standard_readFile_fails :: Assertion
unit_standard_readFile_fails = withTestFile str $ \fp ->
    try (T.readFile fp) >>= \case
      Right _ -> assertFailure "Standard `readFile` should fail."
      Left (_ :: IOException) -> pure ()
  where
    -- We use an escape here because otherwise _both_ tasty-discover
    -- and hedgehog fail when they are reading this file for their
    -- discovery or nice error reporting purposes... I know, right?
    str = "doma\285e"


hprop_readFile :: Property
hprop_readFile = property $ do
    str <- forAll $ G.text (R.linear 0 1000) G.unicode
    str' <- liftIO $ withTestFile str (Utf8.readFile >=> evaluate . force)
    str === str'

hprop_writeFile :: Property
hprop_writeFile = property $ do
    str <- forAll $ G.text (R.linear 0 1000) G.unicode
    liftIO $ withTestFile str (flip Utf8.writeFile str)

hprop_openFile :: Property
hprop_openFile = property $ do
    str <- forAll $ G.text (R.linear 0 1000) G.unicode
    str' <- liftIO $ withTestFile str $ \fp -> do
      h <- Utf8.openFile fp IO.ReadMode
      res <- T.hGetContents h
      res' <- evaluate . force $ res
      IO.hClose h
      pure res'
    str === str'

hprop_withFile :: Property
hprop_withFile = property $ do
    str <- forAll $ G.text (R.linear 0 1000) G.unicode
    str' <- liftIO $ withTestFile str $ \fp ->
      Utf8.withFile fp IO.ReadMode $ \h -> do
        res <- T.hGetContents h
        res' <- evaluate . force $ res
        pure res'
    str === str'
