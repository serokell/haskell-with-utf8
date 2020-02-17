{- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

-----------------------------------------------------------------------------
-- |
--
-- Functions in this module will help you make your /executable/ work
-- correctly with encodings of text files and standard handles.
--
-- /Note: if you are developing a library, see "System.IO.Utf8"./
--
-- = Quick start
--
-- Wrap a call to 'withUtf8' around your @main@:
--
-- @
-- import Main.Utf8 (withUtf8)
--
-- main :: IO ()
-- main = 'withUtf8' $ do
--   putStrLn "Hello, мир!"
-- @
--
-- Basically, this is all you have to do for a program that uses
-- @stdin@ and @stdout@ to interact with the user. However, some
-- programs read input from and write output to files and,
-- at the same time, allow the user to redirect @stdin@ and @stdout@
-- instead of providing explicit file names.
--
-- If this is the case for your executable, you should also wrap
-- @Utf8.@'System.IO.Utf8.withHandle' around the code that passes
-- the handle to a third-party library. It is not necessary to do
-- when passing it to your own library, assuming that it follows
-- the recommendations from the documentation of "System.IO.Utf8".
module Main.Utf8
  ( withUtf8
  , withStdTerminalHandles
  ) where

import Control.Exception.Safe (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.IO.Encoding (getLocaleEncoding, setLocaleEncoding, utf8)
import System.IO (stderr, stdin, stdout)

import System.IO.Utf8 (withTerminalHandle)


-- | Make standard handles safe to write anything to them and change
-- program-global default file handle encoding to UTF-8.
--
-- This function will:
--
--   1. Adjust the encoding of 'stdin', 'stdout', and 'stderr' to
--      enable transliteration, like 'withStdTerminalHandles' does.
--   2. Call 'setLocaleEncoding' to change the program-global locale
--      encoding to UTF-8.
--   3. Undo everything when the wrapped action finishes.
withUtf8 :: (MonadIO m, MonadMask m) => m r -> m r
withUtf8 act = withStdTerminalHandles $
  bracket
    (liftIO $ getLocaleEncoding <* setLocaleEncoding utf8)
    (liftIO . setLocaleEncoding)
    (\_ -> act)

-- | Make standard handles safe to write anything to them.
--
-- This function will for each of 'stdin', 'stdout', 'stderr' do:
--
--   1. Tweak the existing encoding so that unrepresentable characters
--      will get approximated (aka transliterated) by visually similar
--      ones or question marks.
--   2. Restore the original encoding when the wrapped action finishes.
--
-- Use this function only if you do not want to change the program-global
-- locale encoding. Otherwise prefer 'withUtf8'.
withStdTerminalHandles :: (MonadIO m, MonadMask m) => m r -> m r
withStdTerminalHandles
  = withTerminalHandle stdin
  . withTerminalHandle stdout
  . withTerminalHandle stderr
