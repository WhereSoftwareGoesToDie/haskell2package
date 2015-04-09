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
    } deriving Show

data PackagerInfo = PackagerInfo
    { target        :: String
    , buildNoString :: String
    , packageName   :: Maybe String
    , cabalInfo     :: CabalInfo
    , anchorRepos   :: Set String
    , sysDeps       :: Set String
    , anchorDeps    :: Set String
    } deriving Show

type Packager = ReaderT PackagerInfo IO
