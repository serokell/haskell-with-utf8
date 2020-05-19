{- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE LambdaCase   #-}

-----------------------------------------------------------------------------
-- |
--
-- Standard IO functions assume that the character encoding of the data
-- they read or write is the same as the one used by current locale. In many
-- situtations this assumption is wrong, as tools work with files, and
-- files nowadays are mostly UTF-8 encoded, regardless of the locale.
-- Therefore, it is almost always a good idea to switch the encoding of
-- file handles to UTF-8.
--
-- The same applies to standard input, output, and error handles. However,
-- there is an edge-case: if they are attached to a terminal, and the
-- encoding is not UTF-8, using UTF-8 might actually be unsafe.
--
-- If you are developing an executable, in most cases, it is enough to
-- configure the environment accordingly on program start, see the
-- "Main.Utf8" for functions that help with this.
-- However, if you are a library author, you should avoid modifying the
-- global environment.
--
-- = Quick start
--
-- == Opening new files
--
-- If you need to open a text file, use @Utf8.@'withFile'
-- (or @Utf8.@'openFile'). These will not only open the file, but also
-- set the handle’s encoding to UTF-8, regardless of the user’s locale.
--
-- == Working with existing handles
--
-- Suppose you are creating a function which produces some text and writes
-- it to a file handle that is passed to it from the outside.
-- Ask yourself this question: do I want to encode this text in UTF-8
-- or using the encoding from the user’s locale?
--
-- In many cases this question is easy to answer. For example, if your
-- function produces Haskell code, then you always want it in UTF-8,
-- because that is what all other tools (including GHC) expect.
--
-- In some cases it is not that clear. What you can do then is consider
-- what the user is going to do with the data produced.
-- If it is, primarily, meant to be displayed on their screen and then
-- forgotten, you don’t need UTF-8. On the other hand, if it is meant
-- to be saved somewhere and then used or edited by other tools, then
-- you need UTF-8.
--
-- If you decided that your function needs to try to switch the handle
-- to UTF-8, it is very easy to achieve:
--
-- @
-- import qualified System.IO.Utf8 as Utf8
--
-- writeData :: 'IO.Handle' -> InputDataType -> IO ()
-- writeData hOut inData = Utf8.'withHandle' hOut $ do
--   {- ... write the data ...  -}
-- @
--
-- If you decided that you don’t need to try to switch it to UTF-8,
-- replace @withHandle@ with 'withTerminalHandle' to only make the
-- handle safe to write to without runtime errors.
module System.IO.Utf8
  ( withHandle
  , withTerminalHandle

  , setHandleEncoding
  , setTerminalHandleEncoding

  , openFile
  , withFile
  ) where

import Control.Exception.Safe (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor (void)
import GHC.IO.Encoding (mkTextEncoding, utf8)

import qualified System.IO as IO

import System.IO.Utf8.Internal (EncodingAction (..), chooseBestEnc)


type EncRestoreAction m = IO.Handle -> m ()

-- | Set the best available UTF-8-compatible encoding for the handle.
-- Returns the action that will restore the previous one.
--
-- If the handle is in binary mode, does nothing.
-- If the handle is not attached to a terminal, sets UTF-8.
-- Otherwise, keeps its current encoding, but augments it to transliterate
-- unsupported characters.
hSetBestUtf8Enc
  :: MonadIO m
  => (IO.Handle -> IO Bool)
  -> IO.Handle
  -> m (EncRestoreAction m)
hSetBestUtf8Enc hIsTerm h = liftIO $ do
    IO.hGetEncoding h >>= chooseBestEnc h hIsTerm >>= \case
      Keep -> pure (\_ -> pure ())
      ChangeFromTo enc newName -> do
        mkTextEncoding newName >>= IO.hSetEncoding h
        pure $ liftIO . flip IO.hSetEncoding enc


-- | Set handle encoding to the best possible.
--
-- See 'withHandle' for description and prefer it, if possible.
setHandleEncoding :: MonadIO m => IO.Handle -> m ()
setHandleEncoding = liftIO . void . hSetBestUtf8Enc IO.hIsTerminalDevice

-- | Temporarily set handle encoding to the best possible.
--
-- “Best possible” means UTF-8, unless the handle points to a terminal
-- device, in which case the encoding will be left the same, but tweaked
-- to approximate unencodable characters.
--
-- This function is safe to call on handles open in binary mode and it will
-- do nothing on them.
--
-- To sum up:
--
--   * If the handle is in binary mode, do nothing.
--   * If the handle points to a terminal device, act like 'withTerminalHandle'.
--   * For regular files always choose UTF-8, of course.
withHandle :: (MonadIO m, MonadMask m) => IO.Handle -> m r -> m r
withHandle h = bracket (hSetBestUtf8Enc IO.hIsTerminalDevice h) ($ h) . const

-- | Make a handle safe to write any text to.
--
-- See 'withTerminalHandle' for description and prefer it, if possible.
setTerminalHandleEncoding :: MonadIO m => IO.Handle -> m ()
setTerminalHandleEncoding = liftIO . void . hSetBestUtf8Enc (const $ pure True)

-- | Temporarily make a handle safe to write any text to.
--
-- If the handle is not using UTF-8, adjust the encoding to remain the same
-- as before, but approximate unencodable characters. When the action is done,
-- restore it back to the previous one.
--
-- Use this function only if you are sure you want to treat this handle as
-- a terminal (that is, you will be using it to interact with the user
-- and to write user-visible messages, rather than something that can
-- be reasonably expected to go to a file).
--
-- This function is safe to call on handles open in binary mode and it will
-- do nothing on them.
withTerminalHandle :: (MonadIO m, MonadMask m) => IO.Handle -> m r -> m r
withTerminalHandle h = bracket (hSetBestUtf8Enc (const $ pure True) h) ($ h) . const


-- | Like 'System.IO.openFile', but sets the file encoding to UTF-8, regardless
-- of the current locale.
openFile :: MonadIO m => IO.FilePath -> IO.IOMode -> m IO.Handle
openFile path mode = liftIO $ do
  h <- IO.openFile path mode
  IO.hSetEncoding h utf8
  pure h

-- | Like 'System.IO.withFile', but sets the file encoding to UTF-8, regardless
-- of the current locale.
withFile
  :: (MonadIO m, MonadMask m)
  => IO.FilePath -> IO.IOMode -> (IO.Handle -> m r) -> m r
withFile path mode = bracket (openFile path mode) (liftIO . IO.hClose)
