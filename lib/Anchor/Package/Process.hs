{-# LANGUAGE RecordWildCards #-}

module Anchor.Package.Process where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                              as S
import           Data.Version
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.Verbosity
import           Github.Auth
import           Github.Repos
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.Process

import           Anchor.Package.Template
import           Anchor.Package.Types

packageDebian :: IO ()
packageDebian = do
    packagerInfo <- genPackagerInfo
    flip runReaderT packagerInfo $ do
        PackagerInfo{..} <- ask
        let CabalInfo{..} = cabalInfo
        let executablePaths = map (\x -> "dist/build" </> x </> x) executableNames
        liftIO $ do
            createDirectoryIfMissing True $ workspacePath </> "packages"
            createDirectoryIfMissing True $ target </> "debian/usr/bin"
            createDirectoryIfMissing True $ target </> "debian/DEBIAN"
            setCurrentDirectory target
            callProcess "cabal" ["update"]
            callProcess "cabal" ["sandbox", "init"]
            case S.toList anchorDeps of
                [] -> return ()
                ds -> callProcess "cabal" $ ["sandbox", "add-source"] <> map ("../" <>) ds
            callProcess "cabal" ["install", "--only-dependencies", "--enable-tests", "-j"]
            callProcess "cabal" ["configure", "--enable-tests"]
            callProcess "cabal" ["test"]
            callProcess "cabal" ["build"]
            deps <- getSysDeps executablePaths
            control <- runReaderT generateControlFile packagerInfo{sysDeps=S.fromList deps}
            let controlPath = "debian/DEBIAN/control"
            writeFile controlPath control
            forM_ executablePaths
                (\x -> callProcess "cp" [x, "debian/usr/bin/"])
            hasExtraFiles <- doesDirectoryExist "files"
            when hasExtraFiles $ do
                createDirectoryIfMissing True $ "debian/usr/share" </> target
                void $ system $ "cp files/* -a debian/usr/share" </> target
            setCurrentDirectory "debian"
            md5s <- readProcess "bash" ["-c", "find -type f -print0 | xargs -0 md5sum | sed -r \"s# \\./# #\""] ""
            writeFile "DEBIAN/md5sums" md5s
            setCurrentDirectory ".."
            callProcess "dpkg-deb" ["--build", "debian"]
            let outputName = fromMaybe target packageName
            callProcess "mv" ["debian.deb", workspacePath </> "packages" </> outputName <> "_" <> versionString <> "-" <> buildNoString <> "_amd64.deb"]

  where
    getSysDeps :: [String] -> IO [String]
    getSysDeps executablePaths = do
        libs <- readProcess "bash" ["-c", "ldd " <> unwords executablePaths <> " | awk '/=>/{print $(NF-1)}'"] ""
        let libs' = nub . sort . map takeFileName . lines $ libs
        pkgs <- readProcess "dpkg" ("-S" : libs') ""
        return (sort . nub . fmap (takeWhile (/= ':')) . lines $ pkgs)

packageCentos :: IO ()
packageCentos = do
    packagerInfo <- genPackagerInfo
    flip runReaderT packagerInfo $ do
        PackagerInfo{..} <- ask
        installSysDeps
        spec <- generateSpecFile
        liftIO $ do
            let specPath = target </> target <> ".spec"
            writeFile specPath spec
            createDirectoryIfMissing True (homePath </> "rpmbuild/SOURCES/")
            callProcess "mv" [target <> "/../*.tar.gz", homePath <> "/rpmbuild/SOURCES/"]
            callProcess "rpmdev-setuptree" []
            writeFile (homePath </> ".rpmmacros") "%debug_package %{nil}"
            callProcess "rpmbuild"
                    [ "-bb"
                    , "--define"
                    , "build_number " <> buildNoString
                    , "--define"
                    , "dist .el7"
                    , specPath
                    ]
            createDirectoryIfMissing True $ workspacePath <> "/packages"
            void $ system $ "cp " <> homePath </> "rpmbuild/RPMS/x86_64/*.rpm " <> workspacePath </> "packages/"
  where
    installSysDeps = do
        deps <- map (<> "-devel") <$> (<> ["gmp", "zlib"]) <$> S.toList <$> sysDeps <$> ask
        liftIO $ forM_ ("m4" : deps) $ \dep ->
            callProcess "sudo" ["yum", "install", "-y", dep]

genPackagerInfo :: IO PackagerInfo
genPackagerInfo = do
    setEnv "LANG" "en_US.UTF-8"
    sysDeps <- S.fromList <$> getArgs
    token <- strip <$> getEnv "OAUTH_TOKEN"
    buildNoString <- getEnv "BUILD_NUMBER"
    jobName <- getEnv "JOB_NAME"
    target <- fromMaybe jobName <$> lookupEnv "H2P_TARGET"
    packageName <- lookupEnv "H2P_PACKAGE_NAME"
    homePath <- getEnv "HOME"
    workspacePath <- getEnv "WORKSPACE"
    anchorRepos <- getAnchorRepos token
    cabalInfo   <- extractCabalDetails (cabalPath target)
    anchorDeps  <- cloneAndFindDeps target anchorRepos
    return PackagerInfo{..}
  where
    strip :: String -> String
    strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    getAnchorRepos :: String -> IO (S.Set String)
    getAnchorRepos token = do
        res <- organizationRepos' (Just $ GithubOAuth token) "anchor"
        case res of
            Left e -> fail $ show e
            Right xs -> return . S.fromList . fmap repoName $ xs
    cloneAndFindDeps :: String -> S.Set String -> IO (S.Set String)
    cloneAndFindDeps target anchorRepos = do
        startingDeps <- (\s -> s `S.difference` S.singleton target) <$>
                            findCabalBuildDeps (cabalPath target) anchorRepos
        archiveCommand target
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

        cloneCommand   pkg = callProcess
                                    "git"
                                    [ "clone"
                                    , "git@github.com:anchor" </> pkg <> ".git"
                                    ]

        archiveCommand pkg = do
            res <- archiveCommand' pkg
            case res of
                ExitSuccess -> return ()
                e@(ExitFailure _) -> fail $ show e
        archiveCommand' pkg = waitForProcess =<< runProcess
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

    findCabalBuildDeps :: FilePath -> S.Set String -> IO (S.Set String)
    findCabalBuildDeps fp anchorRepos = do
        gpd <- readPackageDescription deafening fp
        return $ S.intersection anchorRepos $ S.fromList $ concat $ concat
            [ map extractDeps $ maybeToList $ condLibrary gpd
            , map (extractDeps . snd) (condExecutables gpd)
            , map (extractDeps . snd) (condTestSuites gpd)
            ]

    extractDeps = map (\(Dependency (PackageName n) _ ) -> n) . condTreeConstraints

extractDefaultCabalDetails :: IO CabalInfo
extractDefaultCabalDetails = do
    target <- (!! 0) <$> getArgs
    extractCabalDetails (cabalPath target)

cabalPath :: String -> FilePath
cabalPath target = target <> "/" <> target <> ".cabal"

extractCabalDetails :: FilePath -> IO CabalInfo
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
