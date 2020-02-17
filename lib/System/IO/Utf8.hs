{- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE LambdaCase   #-}

-- | System IO for the modern world.
--
-- Standard IO functions assume that the character encoding of the data
-- they read or write is the same as the one used by current locale. In many
-- situtations this assumption is wrong, as tools work with files, and
-- the files nowadays are mostly UTF-8 encoded, regardless of the locale.
-- Therefore, it is almost always a good idea to switch the encoding of
-- file handles to UTF-8.
--
-- The same applies to standard input, output, and error handles. However,
-- there is an edge-case: if they are attached to a terminal, and the
-- encoding is not UTF-8, using UTF-8 might actually be unsafe.
--
-- Functions in this module help deal with all these issues.
module System.IO.Utf8
  ( withUtf8StdHandles

  , hSetEncoding
  , hWithEncoding

  , openFile
  , withFile
  ) where

import Control.Exception.Safe (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor (void)
import GHC.IO.Encoding (mkTextEncoding)
import System.IO (stderr, stdin, stdout)

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
hSetBestUtf8Enc :: MonadIO m => IO.Handle -> m (EncRestoreAction m)
hSetBestUtf8Enc h = liftIO $ do
    IO.hGetEncoding h >>= chooseBestEnc h >>= \case
      Keep -> pure (\_ -> pure ())
      ChangeFromTo enc newName -> do
        mkTextEncoding newName >>= IO.hSetEncoding h
        pure $ liftIO . flip IO.hSetEncoding enc

-- | Configures the encodings of the three standard handles (stdin, stdout, stderr)
-- to work with UTF-8 encoded data and runs the specified IO action.
-- After the action finishes, restores the original encodings.
withUtf8StdHandles :: IO a -> IO a
withUtf8StdHandles action =
    hWithEncoding stdin $
    hWithEncoding stdout $
    hWithEncoding stderr $
      action


-- | Set handle encoding to the best possible.
--
-- It is safe to call this function on any kind of handle whatsoever.
--
--   * If the handle is in binary mode, it will do nothing.
--   * If the handle is a terminal, it will use the same encoding, but switch
--     it to Unicode approximation mode so it won't throw errors on invalid
--     byte sequences, but instead try to approximate unecodable characters
--     with visually similar encodable ones.
--   * For regular files it will always choose UTF-8, of course.
--
-- You probably shouldn't be using this function. If you open the file
-- yourself, use 'openFile' (or, even better, 'withFile') instead.
-- If you get the handle from somewhere else, use 'hWithEncoding',
-- which will restore the previous encoding when you are done.
hSetEncoding :: MonadIO m => IO.Handle -> m ()
hSetEncoding = liftIO . void . hSetBestUtf8Enc

-- | Temporarily set handle encoding to the best possible.
--
-- This is like 'hSetEncoding', but it will restore the encoding
-- to the previous one when the action is done.
hWithEncoding :: (MonadIO m, MonadMask m) => IO.Handle -> m r -> m r
hWithEncoding h = bracket (hSetBestUtf8Enc h) ($ h) . const


-- | Like @openFile@, but sets the file encoding to UTF-8, regardless
-- of the current locale.
openFile :: MonadIO m => IO.FilePath -> IO.IOMode -> m IO.Handle
openFile path mode = do
  h <- liftIO $ IO.openFile path mode
  hSetEncoding h
  pure h

-- | Like @withFile@, but sets the file encoding to UTF-8, regardless
-- of the current locale.
withFile
  :: (MonadIO m, MonadMask m)
  => IO.FilePath -> IO.IOMode -> (IO.Handle -> m r) -> m r
withFile path mode = bracket (openFile path mode) (liftIO . IO.hClose)
