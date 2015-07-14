{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Anchor.Package.Process where

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Char
import           Data.List
import qualified Data.Map                              as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                              as S
import           Data.Version
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.Simple.Compiler
import           Distribution.Simple.GHC
import           Distribution.Simple.PackageIndex
import           Distribution.Simple.Program
import           Distribution.Verbosity
import           Github.Auth
import           Github.Repos
import           Github.Search
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
        let dataDir = "debian/usr/share" </> name
        liftIO $ do
            createDirectoryIfMissing True $ workspacePath </> "packages"
            createDirectoryIfMissing True $ target </> "debian/usr/bin"
            createDirectoryIfMissing True $ target </> dataDir
            createDirectoryIfMissing True $ target </> "debian/DEBIAN"
            callProcess "cabal" ["update"]
            setCurrentDirectory target
            callProcess "cabal" ["sandbox", "init"]
            case S.toList anchorDeps of
                [] -> return ()
                ds -> callProcess "cabal" $ ["sandbox", "add-source"] <> map ("../" <>) ds
            callProcess "cabal" ["install", "--only-dependencies", "--enable-tests", "-j"]
            callProcess "cabal" ["configure", "--enable-tests"]
            callProcess "cabal" ["test"]
            callProcess "cabal" ["build"]
            deps <- S.union sysDeps . S.fromList <$> getSysDeps executablePaths
            control <- runReaderT generateControlFile packagerInfo{sysDeps=deps}
            let controlPath = "debian/DEBIAN/control"
            writeFile controlPath control
            forM_ executablePaths
                (\x -> callProcess "cp" [x, "debian/usr/bin/"])
            forM_ dataFileNames
                (\x -> callProcess "cp" [x, dataDir])
            exists <- doesDirectoryExist "scripts"
            when exists $
                callCommand "find scripts -type f -executable -exec cp {} debian/usr/bin/ \\;"
            hasExtraFiles <- doesDirectoryExist "files"
            when hasExtraFiles $ do
                createDirectoryIfMissing True $ "debian/usr/share" </> target
                void $ system $ "cp files/* -a debian/usr/share" </> target
            setCurrentDirectory "debian"
            md5s <- readProcess "bash" ["-c", "find -type f -print0 | xargs -0 md5sum | sed -r \"s# \\./# #\""] ""
            writeFile "DEBIAN/md5sums" md5s
            setCurrentDirectory ".."
            callProcess "dpkg-deb" ["--build", "debian"]
            let outputName = fromMaybe (takeFileName target) packageName
            callProcess "mv" ["debian.deb", workspacePath </> "packages" </> outputName <> "_" <> versionString <> "-" <> buildNoString <> "_amd64.deb"]

  where
    getSysDeps :: [String] -> IO [String]
    getSysDeps executablePaths = do
        libs <- readProcess "bash" ["-c", "ldd " <> unwords executablePaths <> " | awk '/=> \\//{print $(NF-1)}'"] ""
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
            let specPath = takeBaseName target <.> "spec"
            writeFile specPath spec
            createDirectoryIfMissing True (homePath </> "rpmbuild/SOURCES/")
            callCommand $ unwords ["mv", workspacePath </> "*.tar.gz", homePath <> "/rpmbuild/SOURCES/"]
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
            callCommand $ "cp " <> homePath </> "rpmbuild/RPMS/x86_64/*.rpm " <> workspacePath </> "packages/"
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
    anchorRepos <- getAnchorRepos (GithubOAuth token)
    cabalInfo   <- extractCabalDetails (cabalPath target)
    (_,_,conf) <- configure deafening Nothing Nothing defaultProgramConfiguration
    installed   <- getInstalledPackages deafening [GlobalPackageDB, UserPackageDB] conf
    anchorDeps  <- cloneAndFindDeps target workspacePath installed anchorRepos
    return PackagerInfo{..}
  where
    strip :: String -> String
    strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    getAnchorRepos :: GithubAuth -> IO (M.Map String (S.Set FilePath))
    getAnchorRepos = go mempty 1
      where
        go :: M.Map String (S.Set FilePath) -> Int -> GithubAuth -> IO (M.Map String (S.Set FilePath))
        go repos page token = do
            res <- searchCode' (Just token) ("q=user:anchor+extension:cabal&page=" <> show page)
            case res of
                Left e -> fail $ show e
                Right (SearchCodeResult {searchCodeCodes = []}) -> return repos
                Right (SearchCodeResult {searchCodeCodes = codes}) ->
                    go (foldl' addCode repos codes) (page+1) token
        addCode :: M.Map String (S.Set FilePath) -> Code -> M.Map String (S.Set FilePath)
        addCode repos Code{..} = M.insertWith (<>) (repoName codeRepo) (S.singleton $ dropWhile (=='/') codePath) repos
    cloneAndFindDeps :: String -> FilePath -> PackageIndex -> M.Map String (S.Set FilePath) -> IO (S.Set FilePath)
    cloneAndFindDeps target workspacePath installed anchorRepos = do
        let anchorRepos' = do
                (repo,cabal_files) <- M.toList anchorRepos
                cabal_file <- S.toList cabal_files
                return (PackageName $ takeBaseName cabal_file, (repo,cabal_file,False))
        startingDeps <- findCabalBuildDeps (cabalPath target)
        archiveCommand target workspacePath
        loop (M.fromList anchorRepos') startingDeps
      where
        loop anchorRepos' missing
            | S.null missing = return . S.fromList $ do
                  (repo,cabal_file,True) <- M.elems anchorRepos'
                  return $ repo </> takeDirectory cabal_file
            | otherwise = do
                  let x = S.elemAt 0 missing
                      missing' = S.deleteAt 0 missing
                  case (lookupPackageName installed x, M.lookup x anchorRepos') of
                      (_:_,_) -> loop anchorRepos' missing'
                      (_,Nothing) -> loop anchorRepos' missing'
                      (_,Just (_,_,True)) -> loop anchorRepos' missing'
                      (_,Just (repo,cabal_file,False)) -> do
                          cloneCommand repo
                          new_missing <- findCabalBuildDeps $ repo </> cabal_file
                          loop (M.insert x (repo,cabal_file,True) anchorRepos') (missing' <> new_missing)

        cloneCommand repo = do
            exists <- doesDirectoryExist repo
            unless exists $ callProcess
                "git"
                [ "clone"
                , "git@github.com:anchor" </> repo <.> "git"
                ]

        archiveCommand pkg path = do
            res <- archiveCommand' pkg path
            case res of
                ExitSuccess -> return ()
                e@(ExitFailure _) -> fail $ show e

        archiveCommand' pkg path = waitForProcess =<< runProcess
                                    "git"
                                    [ "archive"
                                    , "--prefix=" <> takeBaseName pkg <> "/"
                                    , "-o"
                                    , path </> takeBaseName pkg <.> "tar.gz"
                                    , "HEAD"
                                    ]
                                    (Just pkg)
                                    Nothing
                                    Nothing
                                    Nothing
                                    Nothing

    findCabalBuildDeps :: FilePath -> IO (S.Set PackageName)
    findCabalBuildDeps fp = do
        GenericPackageDescription{..} <- readPackageDescription deafening fp
        return . S.delete (pkgName . package $ packageDescription) . S.fromList . concat . concat $
            [ map extractDeps . maybeToList $ condLibrary
            , map (extractDeps . snd) condExecutables
            , map (extractDeps . snd) condTestSuites
            ]

    extractDeps = map (\(Dependency n _ ) -> n) . condTreeConstraints

extractDefaultCabalDetails :: IO CabalInfo
extractDefaultCabalDetails = do
    target <- (!! 0) <$> getArgs
    extractCabalDetails (cabalPath target)

cabalPath :: FilePath -> FilePath
cabalPath target = target </> takeBaseName target <.> "cabal"

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
             (dataFiles   pd)
