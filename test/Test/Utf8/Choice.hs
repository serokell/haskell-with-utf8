{- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Utf8.Choice where

import GHC.IO.Encoding (char8, latin1, textEncodingName, utf8)
import System.IO.Temp (withSystemTempFile)

import qualified System.IO as IO

import Test.HUnit (Assertion, assertFailure)
import Test.Tasty.HUnit ((@=?))

import System.IO.Utf8.Internal (EncodingAction (..), chooseBestEnc, chooseBestEncPure)

import qualified System.IO.Utf8 as Utf8

import Test.Util (withStdoutIn, withTmpFileIn)


-- | Compare @chooseBestEnc@ with @chooseBestEncPure@ on the handle.
verifyOn :: IO.Handle -> Assertion
verifyOn h = do
  isTerm <- IO.hIsTerminalDevice h
  enc <- IO.hGetEncoding h

  let pureResult = chooseBestEncPure isTerm (textEncodingName <$> enc)
  realResult <- chooseBestEnc h enc

  case (pureResult, realResult) of
    (Nothing, Keep) -> pure ()
    (Just pureName, ChangeFromTo _ realName) -> do
      pureName @=? realName
    (_, _) -> assertFailure "Action mismatch"


unit_pure_binary_file :: Assertion
unit_pure_binary_file =
  Nothing @=? chooseBestEncPure False Nothing

unit_pure_binary_term :: Assertion
unit_pure_binary_term =
  Nothing @=? chooseBestEncPure True Nothing

unit_pure_term_ascii :: Assertion
unit_pure_term_ascii =
  Just "ASCII//TRANSLIT" @=? chooseBestEncPure True (Just "ASCII")

unit_pure_term_utf8 :: Assertion
unit_pure_term_utf8 =
  Nothing @=? chooseBestEncPure True (Just "UTF-8")

unit_pure_file_ascii :: Assertion
unit_pure_file_ascii =
  Just "UTF-8" @=? chooseBestEncPure False (Just "ASCII")

unit_pure_file_utf8 :: Assertion
unit_pure_file_utf8 =
  Nothing @=? chooseBestEncPure False (Just "UTF-8")

unit_pure_idempotent :: Assertion
unit_pure_idempotent =
  Nothing @=? chooseBestEncPure True (Just "ASCII//TRANSLIT")


unit_stdout_utf8 :: Assertion
unit_stdout_utf8 = withStdoutIn utf8 verifyOn

unit_stdout_char8 :: Assertion
unit_stdout_char8 = withStdoutIn char8 verifyOn

unit_stdout_latin1 :: Assertion
unit_stdout_latin1 = withStdoutIn latin1 verifyOn


unit_file_binary :: Assertion
unit_file_binary = withSystemTempFile "utf8.bin" $ \_ h -> do
  IO.hSetBinaryMode h True
  verifyOn h

unit_file_utf8 :: Assertion
unit_file_utf8 = withTmpFileIn utf8 verifyOn

unit_file_char8 :: Assertion
unit_file_char8 = withTmpFileIn char8 verifyOn

unit_file_latin1 :: Assertion
unit_file_latin1 = withTmpFileIn latin1 verifyOn

unit_stdout_idempotent :: Assertion
unit_stdout_idempotent = withStdoutIn char8 $ \h -> do
  Just enc <- IO.hGetEncoding h
  "ISO-8859-1" @=? textEncodingName enc  -- sanity check
  Utf8.hWithEncoding h $ do
    -- XXX: Actually not true, there is no suffix in the name
    -- Just enc' <- IO.hGetEncoding h
    -- "ISO-8859-1//TRANSLIT" @=? textEncodingName enc'  -- sanity check
    verifyOn h
