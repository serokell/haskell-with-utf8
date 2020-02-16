{- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | The missing MonadMask instance for Hedgehog.
module Test.Util.Hedgehog
  () where

{-
import Control.Exception.Safe (MonadMask (..))

import Hedgehog.Internal.Gen
import Hedgehog.Internal.Property
import Hedgehog.Internal.Tree

import qualified Hedgehog.Internal.Seed as Seed


deriving newtype instance MonadMask m => MonadMask (TestT m)
deriving newtype instance MonadMask m => MonadMask (PropertyT m)

instance MonadMask m => MonadMask (GenT m) where
  mask a = GenT $ \size seed ->
    mask $ \u -> runGenT size seed (a $ mapGenT u)

  uninterruptibleMask a = GenT $ \size seed ->
    uninterruptibleMask $ \u -> runGenT size seed (a $ mapGenT u)

  generalBracket acquire release use = GenT $ \size seed ->
    case Seed.split seed of
      (seed1, seed1') -> case Seed.split seed1' of
        (seed2, seed3) -> do
          generalBracket
            (runGenT size seed1 acquire)
            (\treeRes ec -> runGenT size seed2 (release treeRes ec))
            (\treeRes -> runGenT size seed3 (use treeRes))

instance MonadMask m => MonadMask (TreeT m) where
  mask a
    = TreeT $ mask $ \u -> runTreeT (a $ mapTreeT u)

  uninterruptibleMask a
    = TreeT $ uninterruptibleMask $ \u -> runTreeT (a $ mapTreeT u)

  generalBracket acquire release use = undefined
-}
