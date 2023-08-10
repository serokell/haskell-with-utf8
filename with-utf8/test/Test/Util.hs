{- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

-- | Testing helpers.
module Test.Util
  ( withTerminalIn
  , withTmpFileIn
  ) where

import Control.Exception.Safe (bracket, finally)
import Control.Monad (void)
import GHC.IO.Encoding (TextEncoding)
import System.IO.Temp (withSystemTempFile)
import System.Posix.IO (closeFd, fdToHandle, handleToFd)
import System.Posix.Terminal (openPseudoTerminal)

import qualified System.IO as IO


-- | Make a new pseudo-terminal with the given encoding.
withTerminalIn :: TextEncoding -> (IO.Handle -> IO r) -> IO r
withTerminalIn enc act = bracket
    openPseudoTerminal
    (\(fd1, fd2) -> closeFd fd2 `finally` closeFd fd1)
    (\(fd, _) -> bracket
      (fdToHandle fd)
      (void . handleToFd)  -- just release the fd from the handle
      (\h -> IO.hSetEncoding h enc *> act h)
    )

-- | Make a temp file with the given encoding.
withTmpFileIn :: TextEncoding -> (IO.Handle -> IO r) -> IO r
withTmpFileIn enc act = withSystemTempFile "utf8.txt" $ \_ h -> do
  IO.hSetEncoding h enc
  act h
