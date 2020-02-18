{- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Utf8.Set where

import GHC.IO.Encoding (char8, latin1, textEncodingName, utf8)

import qualified System.IO as IO

import Test.HUnit (Assertion)
import Test.Tasty.HUnit ((@=?))

import System.IO (hIsTerminalDevice)
import System.IO.Utf8.Internal (EncodingAction (..), chooseBestEnc)

import qualified System.IO.Utf8 as Utf8

import Test.Util (withTmpFileIn)


-- | Check that the encoding was actually set to the right one.
verifyOn :: IO.Handle -> Assertion
verifyOn h = do
  menc <- IO.hGetEncoding h
  act <- chooseBestEnc h hIsTerminalDevice menc

  Utf8.withHandle h $ do
    menc' <- IO.hGetEncoding h
    case act of
      Keep ->
        fmap textEncodingName menc @=? fmap textEncodingName menc'
      ChangeFromTo enc enc' -> do
        fmap textEncodingName menc @=? Just (textEncodingName enc)
        Just enc' @=? fmap textEncodingName menc'

  -- Check that it restores
  menc' <- IO.hGetEncoding h
  fmap textEncodingName menc @=? fmap textEncodingName menc'


-- XXX: This group of tests does not actually work, because there is
--      no way to check that the newly set encoding is transliterating
--      (see the comment in @chooseBestEnc@).

-- unit_term_utf8 :: Assertion
-- unit_term_utf8 = withTerminalIn utf8 verifyOn
--
-- unit_term_char8 :: Assertion
-- unit_term_char8 = withTerminalIn char8 verifyOn
--
-- unit_term_latin1 :: Assertion
-- unit_term_latin1 = withTerminalIn latin1 verifyOn


unit_file_utf8 :: Assertion
unit_file_utf8 = withTmpFileIn utf8 verifyOn

unit_file_char8 :: Assertion
unit_file_char8 = withTmpFileIn char8 verifyOn

unit_file_latin1 :: Assertion
unit_file_latin1 = withTmpFileIn latin1 verifyOn
