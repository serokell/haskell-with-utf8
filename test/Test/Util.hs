{- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

-- | Testing helpers.
module Test.Util
  ( withStdoutIn
  , withTmpFileIn
  ) where

import Control.Exception.Safe (bracket)
import GHC.IO.Encoding (TextEncoding)
import GHC.IO.Handle (hDuplicate)
import System.IO.Temp (withSystemTempFile)

import qualified System.IO as IO


-- | Make a duplicate of @stdout@ with the given encoding.
withStdoutIn :: TextEncoding -> (IO.Handle -> IO r) -> IO r
withStdoutIn enc act = bracket
  (hDuplicate IO.stdout)
  IO.hClose
  (\h -> IO.hSetEncoding h enc *> act h)

-- | Make a temp file with the given encoding.
withTmpFileIn :: TextEncoding -> (IO.Handle -> IO r) -> IO r
withTmpFileIn enc act = withSystemTempFile "utf8.txt" $ \_ h -> do
  IO.hSetEncoding h enc
  act h
