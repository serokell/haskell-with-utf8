{- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

-- | Internal functiona that implement encoding selection logic.
module System.IO.Utf8.Internal
  ( EncodingAction (..)

  , chooseBestEncPure
  , chooseBestEnc
  ) where

import Data.List (isSuffixOf)
import GHC.IO.Encoding (TextEncoding, textEncodingName, utf8)

import qualified System.IO as IO


-- | What to do with the encoding of the handle.
--
-- We return new encoding as string to simplify testing, because,
-- it turns out, when you make an encoding with some name, the
-- resulting encoding can have a different name.
--
-- The second constructor also contains the old encoding as a proof
-- that the encoding is set to a new one only if there was a previous
-- one in the first place. You probably think I am crazy, but I am not,
-- it is just the simplest way to make the @hSetBestUtf8Enc@ easy to write.
data EncodingAction
  = Keep
    -- ^ Do nothing.
  | ChangeFromTo TextEncoding String
    -- ^ Change the first encoding to the second.

-- | Pure version of 'chooseBestUtf8Enc'.
--
-- This function is not actually used in the library. It exists only
-- for documentation purposes to demonstrate the logic.
-- It is also used in tests to verify that the logic implemented is
-- indeed this.
chooseBestEncPure
  :: Bool  -- ^ Is a terminal device.
  -> Maybe String  -- ^ Previous encoding name
  -> Maybe String
-- Never touch handles in binary mode.
chooseBestEncPure _ Nothing = Nothing
-- Already UTF-8, that's cool.
chooseBestEncPure _ (Just "UTF-8") = Nothing
-- Handle in text mode that is not a terminal.
chooseBestEncPure False _ = Just "UTF-8"
chooseBestEncPure True (Just name)
  -- Be idempotent.
  | "//TRANSLIT" `isSuffixOf` name = Nothing
  | otherwise = Just $ name ++ "//TRANSLIT"

-- | Choose the best encoding for a file handle.
--
-- This function implements the same logic as 'chooseBestEncPure',
-- but in a way that is more optimal in terms of IO queries. In
-- particular:
--
-- 1. It receives both a handle and its current encoding, because the
--    calling function will, most likely, need to know the current encoding
--    (e.g. to be able to restore it), so we avoid repeating the query.
-- 2. It first checks for the cases where it doesn't care whether the device
--    is a terminal or not, so the query will be made only if really necessary.
chooseBestEnc :: IO.Handle -> Maybe TextEncoding -> IO EncodingAction
chooseBestEnc _ Nothing = pure Keep
chooseBestEnc h (Just enc) = case textEncodingName enc of
  "UTF-8" -> pure Keep
  name
    -- XXX: The first branch is actually never used, because the encoding
    --      loses the @//TRANSLIT@ suffix after it is being created.
    -- TODO: Find a way to detect that the encoding is already trasliterating.
    | "//TRANSLIT" `isSuffixOf` name -> pure Keep
    | otherwise -> IO.hIsTerminalDevice h >>= \case
        False -> pure $ ChangeFromTo enc (textEncodingName utf8)
        True -> pure $ ChangeFromTo enc (name ++ "//TRANSLIT")
