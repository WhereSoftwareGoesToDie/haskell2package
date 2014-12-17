{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Anchor.Package.Types where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Set               (Set)

data CabalInfo = CabalInfo
    { name              :: String
    , versionString     :: String
    , synopsisString    :: String
    , descriptionString :: String
    , maintainerString  :: String
    , executableNames   :: [String]
    }

data PackagerInfo = PackagerInfo
    { target        :: String
    , outputName    :: String
    , buildNoString :: String
    , cabalInfo     :: CabalInfo
    , anchorRepos   :: Set String
    , sysDeps       :: Set String
    , anchorDeps    :: Set String
    }

newtype Packager a = Packager {
    unPackager :: ReaderT PackagerInfo IO a
} deriving (Functor, Applicative, Monad, MonadReader PackagerInfo, MonadIO)
