{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Stack.GhcPkg
Description : Functions for the GHC package database.
License     : BSD-3-Clause

Functions for the GHC package database.
-}

module Stack.GhcPkg
  ( PackageDbCache
  , createDatabase
  , findGhcPkgField
  , getGlobalDB
  , ghcPkg
  , ghcPkgPathEnvVar
  , loadPackageDbCache
  , mkGhcPackagePath
  , registerIntoCache
  , unregisterGhcPkgIds
  ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Distribution.InstalledPackageInfo
                   ( InstalledPackageInfo, installedUnitId
                   , parseInstalledPackageInfo
                   )
import           Distribution.Package ( mungedId )
import           Distribution.Types.MungedPackageName
                   ( encodeCompatPackageName )
import           Distribution.Types.UnitId ( unUnitId )
import           Path ( (</>), filename, parent )
import           Path.Extra ( toFilePathNoTrailingSep )
import           Path.IO
                   ( doesDirExist, doesFileExist, ensureDir, ignoringAbsence
                   , listDir, removeFile, resolveDir'
                   )
import           RIO.Process ( HasProcessContext, proc, readProcess_ )
import           Stack.Constants ( relFilePackageCache )
import           Stack.Prelude
import           Stack.Types.Compiler ( WhichCompiler (..) )
import           Stack.Types.CompilerPaths ( GhcPkgExe (..) )
import           Stack.Types.GhcPkgId ( GhcPkgId, parseGhcPkgId )
import           System.FilePath ( searchPathSeparator )

-- | Get the global package database
getGlobalDB ::
     (HasProcessContext env, HasTerm env)
  => GhcPkgExe
  -> RIO env (Path Abs Dir)
getGlobalDB pkgexe = do
  logDebug "Getting global package database location"
  -- This seems like a strange way to get the global package database
  -- location, but I don't know of a better one
  bs <- ghcPkg pkgexe [] ["list", "--global"] >>= either throwIO pure
  let fp = S8.unpack $ stripTrailingColon $ firstLine bs
  liftIO $ resolveDir' fp
 where
  stripTrailingColon bs
    | S8.null bs = bs
    | S8.last bs == ':' = S8.init bs
    | otherwise = bs
  firstLine = S8.takeWhile (\c -> c /= '\r' && c /= '\n')

-- | Run the ghc-pkg executable
ghcPkg ::
     (HasProcessContext env, HasTerm env)
  => GhcPkgExe
  -> [Path Abs Dir]
  -> [String]
  -> RIO env (Either SomeException S8.ByteString)
ghcPkg pkgexe@(GhcPkgExe pkgPath) pkgDbs args = do
  eres <- go
  case eres of
    Left e -> do
      prettyDebug $
           fillSep
             [ flow "While using"
             , style Shell "ghc-pkg" <>","
             , flow "Stack encountered the following error:"
             ]
        <> blankLine
        <> string (displayException e)
        <> flow "Trying again after considering database creation..."
      mapM_ (createDatabase pkgexe) pkgDbs
      go
    Right _ -> pure eres
 where
  pkg = toFilePath pkgPath
  go = tryAny $ BL.toStrict . fst <$> proc pkg args' readProcess_
  args' = packageDbFlags pkgDbs ++ args

-- | Create a package database in the given directory, if it doesn't exist.
createDatabase ::
     (HasProcessContext env, HasTerm env)
  => GhcPkgExe
  -> Path Abs Dir
  -> RIO env ()
createDatabase (GhcPkgExe pkgPath) db = do
  exists <- doesFileExist (db </> relFilePackageCache)
  unless exists $ do
    -- ghc-pkg requires that the database directory does not exist
    -- yet. If the directory exists but the package.cache file
    -- does, we're in a corrupted state. Check for that state.
    dirExists <- doesDirExist db
    args <- if dirExists
      then do
        prettyWarnL
          [ flow "The package database located at"
          , pretty db
          , flow "is corrupted. It is missing its"
          , style File "package.cache"
          , flow "file. Stack is proceeding with a recache."
          ]
        pure ["--package-db", toFilePath db, "recache"]
      else do
        -- Creating the parent doesn't seem necessary, as ghc-pkg
        -- seems to be sufficiently smart. But I don't feel like
        -- finding out it isn't the hard way
        ensureDir (parent db)
        pure ["init", toFilePath db]
    void $ proc (toFilePath pkgPath) args $ \pc ->
      onException (readProcess_ pc) $
        logError $
          "Error: [S-9735]\n" <>
           "Unable to create package database at " <>
           fromString (toFilePath db)

-- | Get the environment variable to use for the package DB paths.
ghcPkgPathEnvVar :: WhichCompiler -> Text
ghcPkgPathEnvVar Ghc = "GHC_PACKAGE_PATH"

-- | Get the necessary ghc-pkg flags for setting up the given package database
packageDbFlags :: [Path Abs Dir] -> [String]
packageDbFlags pkgDbs =
    "--no-user-package-db"
  : map (\x -> "--package-db=" ++ toFilePath x) pkgDbs

-- | Get the value of a field of the package.
findGhcPkgField ::
     (HasProcessContext env, HasTerm env)
  => GhcPkgExe
  -> [Path Abs Dir] -- ^ package databases
  -> String -- ^ package identifier, or GhcPkgId
  -> Text
  -> RIO env (Maybe Text)
findGhcPkgField pkgexe pkgDbs name field =
  let cmd = ["field", "--simple-output", name, T.unpack field]
  in  ghcPkg pkgexe pkgDbs cmd <&> \case
        Left _ -> Nothing
        Right bs -> fmap (stripCR . T.decodeUtf8) $ listToMaybe $ S8.lines bs

-- | In-memory cache of a package database's .conf file contents, indexed by
-- both 'PackageIdentifier' and 'GhcPkgId' for efficient lookup during
-- unregistration.
data PackageDbCache = PackageDbCache
  { packageDbCacheByPkgId :: !(Map PackageIdentifier (Set (Path Rel File)))
  , packageDbCacheByGhcPkgId :: !(Map GhcPkgId (Set (Path Rel File)))
  }

emptyPackageDbCache :: PackageDbCache
emptyPackageDbCache = PackageDbCache Map.empty Map.empty

-- | Load the set of .conf files currently in a package database directory,
-- parsed into a 'PackageDbCache' for O(1) lookup when deciding which files to
-- remove during unregistration.
loadPackageDbCache ::
     (HasTerm env)
  => Path Abs Dir
  -> RIO env (TVar PackageDbCache)
loadPackageDbCache pkgDb = do
  dbExists <- doesDirExist pkgDb
  cache <- if not dbExists
    then pure emptyPackageDbCache
    else do
      (_, files) <- listDir pkgDb
      let confFiles = filter isConfFile files
      foldM addConfFile emptyPackageDbCache confFiles
  liftIO $ newTVarIO cache
 where
  isConfFile f =
    let fp = toFilePath f
    in  ".conf" `L.isSuffixOf` fp
  addConfFile acc absFile = do
    let relFile = filename absFile
    contents <- liftIO $ S.readFile (toFilePath absFile)
    case parseInstalledPackageInfo contents of
      Left _ -> pure acc
      Right (_warnings, ipi) ->
        let pidKey = ipiToPackageIdentifier ipi
            relFiles = Set.singleton relFile
            byPkgId =
              Map.insertWith Set.union pidKey relFiles acc.packageDbCacheByPkgId
        in  case ipiToGhcPkgId ipi of
              Nothing -> pure acc { packageDbCacheByPkgId = byPkgId }
              Just gpkgId ->
                let byGpkgId =
                      Map.insertWith Set.union gpkgId relFiles
                        acc.packageDbCacheByGhcPkgId
                in  pure PackageDbCache
                      { packageDbCacheByPkgId = byPkgId
                      , packageDbCacheByGhcPkgId = byGpkgId
                      }

-- | Update the cache after registering new .conf files into the package
-- database. Parses each file to extract identifiers and adds them to both
-- indexes.
registerIntoCache ::
     (HasTerm env)
  => TVar PackageDbCache
  -> [Path Abs File]
  -> RIO env ()
registerIntoCache cacheVar confPaths =
  forM_ confPaths $ \absFile -> do
    let relFile = filename absFile
    contents <- liftIO $ S.readFile (toFilePath absFile)
    case parseInstalledPackageInfo contents of
      Left _ -> pure ()
      Right (_warnings, ipi) -> do
        let pidKey = ipiToPackageIdentifier ipi
            relFiles = Set.singleton relFile
            addEntry cache =
              let byPkgId =
                    Map.insertWith Set.union pidKey relFiles
                      cache.packageDbCacheByPkgId
              in  case ipiToGhcPkgId ipi of
                    Nothing ->
                      cache { packageDbCacheByPkgId = byPkgId }
                    Just gpkgId ->
                      let byGpkgId =
                            Map.insertWith Set.union gpkgId relFiles
                              cache.packageDbCacheByGhcPkgId
                      in  PackageDbCache
                            { packageDbCacheByPkgId = byPkgId
                            , packageDbCacheByGhcPkgId = byGpkgId
                            }
        liftIO $ atomically $ modifyTVar' cacheVar addEntry

-- | Unregister packages from a package database using the in-memory cache for
-- O(1) lookup of .conf files to remove.
--
-- For each 'Left PackageIdentifier': looks up .conf files by package identity.
-- For each 'Right GhcPkgId': looks up .conf files by unit-id.
--
-- After removing the files, calls @ghc-pkg recache@ to rebuild the binary
-- cache.
unregisterGhcPkgIds ::
     (HasProcessContext env, HasTerm env)
  => Bool
     -- ^ Report pretty exceptions as warnings?
  -> GhcPkgExe
  -> TVar PackageDbCache
  -> Path Abs Dir -- ^ package database
  -> NonEmpty (Either PackageIdentifier GhcPkgId)
  -> RIO env ()
unregisterGhcPkgIds isWarn pkgexe cacheVar pkgDb epgids = do
  let (idents, gids) = partitionEithers $ toList epgids
  filesToDelete <- liftIO $ atomically $ do
    cache <- readTVar cacheVar
    let foundByPid =
          foldMap
            (\pid -> Map.findWithDefault Set.empty pid cache.packageDbCacheByPkgId)
            idents
        foundByGid =
          foldMap
            (\gid -> Map.findWithDefault Set.empty gid cache.packageDbCacheByGhcPkgId)
            gids
        allFound = Set.union foundByPid foundByGid
        prunedByPkgId =
          foldl' (flip Map.delete) cache.packageDbCacheByPkgId idents
        prunedByGhcPkgId =
          foldl' (flip Map.delete) cache.packageDbCacheByGhcPkgId gids
    writeTVar cacheVar PackageDbCache
      { packageDbCacheByPkgId = prunedByPkgId
      , packageDbCacheByGhcPkgId = prunedByGhcPkgId
      }
    pure allFound
  forM_ (Set.toList filesToDelete) $ \relFile ->
    ignoringAbsence $ removeFile (pkgDb </> relFile)
  ghcPkg pkgexe [pkgDb] ["recache"] >>= \case
    Left err -> when isWarn $
      prettyWarn $
        "[S-8729]"
        <> line
        <> flow "While recaching after unregistering packages, Stack \
                \encountered the following error:"
        <> blankLine
        <> string (displayException err)
    Right _ -> pure ()

ipiToPackageIdentifier :: InstalledPackageInfo -> PackageIdentifier
ipiToPackageIdentifier ipi =
  let MungedPackageId mn mv = mungedId ipi
  in  PackageIdentifier (encodeCompatPackageName mn) mv

ipiToGhcPkgId :: InstalledPackageInfo -> Maybe GhcPkgId
ipiToGhcPkgId ipi =
  let unitIdStr = T.pack . unUnitId $ installedUnitId ipi
  in  parseGhcPkgId unitIdStr

-- | Get the value for GHC_PACKAGE_PATH
mkGhcPackagePath :: Bool -> Path Abs Dir -> Path Abs Dir -> [Path Abs Dir] -> Path Abs Dir -> Text
mkGhcPackagePath locals localdb deps extras globaldb =
  T.pack $ L.intercalate [searchPathSeparator] $ concat
    [ [toFilePathNoTrailingSep localdb | locals]
    , [toFilePathNoTrailingSep deps]
    , [toFilePathNoTrailingSep db | db <- reverse extras]
    , [toFilePathNoTrailingSep globaldb]
    ]
