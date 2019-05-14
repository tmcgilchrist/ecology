{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Ecology.API.Git
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Ecology.API.Git (
  -- * Types
    GitPlatformAPI(..)
  , GitPlatformAPIs(..)
  ) where

import Irreverent.Ecology.Core.Data (GitRepository, NewGitRepository)

import Ultra.Control.Monad.Trans.Either (EitherT, firstEitherT)

import Preamble

newtype GitPlatformAPIs g a b c m e = GitPlatformAPIs {
    selectGitAPI :: g -> GitPlatformAPI a b c m e
  }

data GitPlatformAPI a b c m e = GitPlatformAPI {
    getOrgRepos   :: EitherT e m [GitRepository]
  , createNewRepo :: NewGitRepository a b -> EitherT e m GitRepository
  , getAuth       :: c
  }

instance (Functor m) => Functor (GitPlatformAPI a b c m) where
--fmap :: (a -> b) -> f a -> f b
  fmap f (GitPlatformAPI gor cnr auth) =
    GitPlatformAPI (firstEitherT f gor) (firstEitherT f . cnr) auth
