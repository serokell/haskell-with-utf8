{- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

-- | "Data.Text.IO" for the modern world.
--
-- Wrappers around simple file reading/writing functions from the
-- @text@ package that reset the handle encoding to UTF-8.
module Data.Text.IO.Utf8
  ( readFile
  , writeFile
  ) where

import Prelude hiding (readFile, writeFile)

import Control.Exception.Safe (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)

import qualified Data.Text.IO as T
import qualified System.IO as IO

import qualified System.IO.Utf8 as Utf8


-- | Like 'T.readFile', but assumes the file is encoded in UTF-8, regardless
-- of the current locale.
readFile :: MonadIO m => IO.FilePath -> m Text
readFile path = Utf8.openFile path IO.ReadMode >>= liftIO . T.hGetContents

-- | Like 'T.writeFile', but encodes the data in UTF-8, regardless
-- of the current locale.
writeFile :: (MonadIO m, MonadMask m) => IO.FilePath -> Text -> m ()
writeFile path = Utf8.withFile path IO.WriteMode . (liftIO .) . flip T.hPutStr
