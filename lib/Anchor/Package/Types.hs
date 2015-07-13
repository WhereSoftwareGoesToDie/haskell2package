module Anchor.Package.Types where

import           Control.Monad.Reader
import           Data.Set             (Set)

data CabalInfo = CabalInfo
    { name              :: String
    , versionString     :: String
    , synopsisString    :: String
    , descriptionString :: String
    , maintainerString  :: String
    , executableNames   :: [String]
    , dataFileNames     :: [String]
    } deriving Show

data PackagerInfo = PackagerInfo
    { target        :: String       -- Name of the target
    , workspacePath :: String       -- Jenkins workspace path
    , buildNoString :: String       -- Jenkins build number
    , packageName   :: Maybe String -- Output package name (without extensions)
    , cabalInfo     :: CabalInfo    -- Parsed cabal information relevant to packaging
    , sysDeps       :: Set String   -- List of system requirements
    , anchorDeps    :: Set String   -- List of anchor-specific dependencies
    , homePath      :: String       -- Jenkins home path
    } deriving Show

type Packager = ReaderT PackagerInfo IO
