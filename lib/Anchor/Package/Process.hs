{-# LANGUAGE RecordWildCards #-}

module Anchor.Package.Process where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Data.Maybe
import           Data.Monoid
import           Data.Set                              (Set)
import qualified Data.Set                              as S
import           Data.String.Utils
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Version
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.Verbosity
import           Distribution.Version
import           Github.Auth
import           Github.Repos
import           System.Directory
import           System.Environment
import           System.IO
import           System.Locale
import           System.Process

import           Anchor.Package.SpecFile
import           Anchor.Package.Types

packageJenkins :: IO ()
packageJenkins = do
    setEnv "LANG" "en_US.UTF-8"
    sysDeps <- S.fromList <$> getArgs
    token <- lookupEnv "OAUTH_TOKEN"
    buildNoString <- getEnv "BUILD_NUMBER"
    target <- getEnv "JOB_NAME"
    homePath <- getEnv "HOME"
    runPackager target buildNoString sysDeps (fmap strip token) (act homePath)
  where
    act homePath = do
        PackagerInfo{..} <- ask
        installSysDeps
        spec <- generateSpecFile "TEMPLATE.spec"
        liftIO $ do
            writeFile (target <> ".spec") spec
            createDirectoryIfMissing True (homePath <> "/rpmbuild/SOURCES/")
            callProcess "mv" ["*.tar.gz", homePath <> "/rpmbuild/SOURCES/"]
            callProcess "rpmdev-setuptree" []
            writeFile (homePath <> "/.rpmmacros") "%debug_package %{nil}"
            callProcess "rpmbuild"
                    [ "-bb"
                    , "--define"
                    , buildNoString
                    , "--define"
                    , "dist .el7"
                    , target <> ".spec"
                    ]
            createDirectoryIfMissing True "packages" 
            callProcess "mv" [homePath <> "rpmbuild/RPMS/x86_64/*.rpm", "packages"]
    installSysDeps =
        fmap (S.toList . anchorDeps) ask >>= \deps -> liftIO $ forM_ deps $
            \dep -> callProcess "sudo" ["yum", "install", "-y", dep <> "-devel"]

runPackager :: String -> String -> Set String -> Maybe String -> Packager a -> IO a
runPackager target buildNoString sysDeps token (Packager act) = do
    anchorRepos <- getAnchorRepos
    cabalInfo   <- extractCabalDetails (cabalPath target)
    anchorDeps  <- cloneAndFindDeps anchorRepos
    let packagerInfo = PackagerInfo target buildNoString cabalInfo anchorRepos sysDeps anchorDeps
    runReaderT act packagerInfo
  where
    cabalPath pkg = concat [pkg, "/", pkg, ".cabal"]
    getAnchorRepos =
        S.fromList <$> either (error . show) (map repoName) <$> organizationRepos' (GithubOAuth <$> token) "anchor"
    extractCabalDetails fp = do
        gpd <- readPackageDescription deafening fp
        let pd = packageDescription gpd
        let (PackageIdentifier (PackageName pName) pVer) = package pd
        return $ CabalInfo
                    pName
                    (showVersion pVer)
                    (synopsis    pd)
                    (description pd)
                    (maintainer  pd)
                    (map fst $ condExecutables gpd)
    cloneAndFindDeps anchorRepos = do
        startingDeps <- (\s -> s `S.difference` S.singleton target) <$> findCabalBuildDeps (cabalPath target) anchorRepos
        fst <$> execStateT (loop startingDeps) (startingDeps, S.empty)
      where
        loop missing = do
            forM_ (S.toList missing) $ \dep -> do
                (fullDeps, iterDeps) <- get
                liftIO $ cloneCommand dep
                liftIO $ archiveCommand dep
                newDeps <- liftIO $ findCabalBuildDeps (cabalPath dep) anchorRepos
                put (fullDeps, newDeps `S.union` iterDeps)
            (fullDeps, iterDeps) <- get
            let missing' = iterDeps `S.difference` fullDeps
            put (fullDeps `S.union` missing', S.empty)
            unless (S.null missing') $ loop missing'
        cloneCommand   pkg = waitForProcess =<< runProcess
                                    "git"
                                    [ "clone"
                                    , "git@github.com:anchor/" <> pkg <> ".git"
                                    ]
                                    Nothing
                                    Nothing
                                    Nothing
                                    Nothing
                                    Nothing
        archiveCommand pkg = waitForProcess =<< runProcess
                                    "git"
                                    [ "archive"
                                    , "--prefix=" <> pkg <> "/"
                                    , "-o"
                                    , "../" <> pkg <> ".tar.gz"
                                    , "HEAD"
                                    ]
                                    (Just pkg)
                                    Nothing
                                    Nothing
                                    Nothing
                                    Nothing
    findCabalBuildDeps fp anchorRepos = do
        gpd <- readPackageDescription deafening fp
        return $ S.intersection anchorRepos $ S.fromList $ concat $ concat
            [ map extractDeps $ maybeToList $ condLibrary gpd
            , map (extractDeps . snd) (condExecutables gpd)
            , map (extractDeps . snd) (condTestSuites gpd)
            ]
    extractDeps = map (\(Dependency (PackageName n) _ ) -> n) . condTreeConstraints
