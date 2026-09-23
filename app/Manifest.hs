{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Manifest is responsible for collecting post metadata and construct
-- | a minimal changes to update output html files.
module Manifest
  ( HomeManifest(..)
  , ListingPost(..)
  , TagCount(..)
  , TaxonomyLink(..)
  , TaxonomyManifest(..)
  , categoryManifest
  , homeManifest
  , listingManifestRoot
  , readHomeManifest
  , readTaxonomyManifest
  , tagManifest
  , writeHomeManifest
  , writeTaxonomyManifest
  ) where

import           Control.Monad              (when)
import           Data.Aeson                 (ToJSON)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Development.Shake         (Action, liftIO, readFile',
                                            writeFileChanged)
import           Development.Shake.FilePath ((</>), (<.>), takeDirectory)
import           GHC.Generics               (Generic)
import           System.Directory           (createDirectoryIfMissing,
                                            doesFileExist)

-- | A single entry in the tag cloud.
data TagCount = TagCount
  { tag    :: String
  , count  :: Int
  , weight :: Int
  , tagUrl :: String
  } deriving (Generic, Read, Show, ToJSON)

-- | A tag or category link rendered from a particular page depth.
data TaxonomyLink = TaxonomyLink
  { taxonomyName :: String
  , taxonomyUrl  :: String
  } deriving (Generic, Read, Show, ToJSON)

-- | The body-free representation of a post used by listing templates.
data ListingPost = ListingPost
  { entryTitle    :: String
  , entryAuthor   :: String
  , listingUrl    :: String
  , entryDate     :: String
  , entryImage    :: Maybe String
  , entryTags     :: [TaxonomyLink]
  , entryCategory :: TaxonomyLink
  } deriving (Generic, Read, Show, ToJSON)

data HomeManifest = HomeManifest
  { homePosts         :: [ListingPost]
  , homeTagCloud      :: [TagCount]
  , homeCategoryCloud :: [TaxonomyLink]
  } deriving (Read, Show)

data TaxonomyManifest = TaxonomyManifest
  { manifestLabel :: String
  , manifestSlug  :: String
  , manifestPosts :: [ListingPost]
  } deriving (Read, Show)

listingManifestRoot :: FilePath
listingManifestRoot = "_build/listings"

homeManifest :: FilePath
homeManifest = listingManifestRoot </> "home.posts"

tagManifest :: String -> FilePath
tagManifest slug = listingManifestRoot </> "tag" </> slug <.> "posts"

categoryManifest :: String -> FilePath
categoryManifest slug =
  listingManifestRoot </> "category" </> slug <.> "posts"

writeHomeManifest
  :: FilePath
  -> [ListingPost]
  -> [TagCount]
  -> [TaxonomyLink]
  -> Action ()
writeHomeManifest out posts tags categories =
  writeManifest out $ HomeManifest
    { homePosts = posts
    , homeTagCloud = tags
    , homeCategoryCloud = categories
    }

writeTaxonomyManifest
  :: FilePath
  -> String
  -> String
  -> [ListingPost]
  -> Action ()
writeTaxonomyManifest out label slug posts =
  writeManifest out $ TaxonomyManifest
    { manifestLabel = label
    , manifestSlug = slug
    , manifestPosts = posts
    }

readHomeManifest :: Action HomeManifest
readHomeManifest =
  readManifest "home listing" homeManifest

readTaxonomyManifest :: FilePath -> Action TaxonomyManifest
readTaxonomyManifest = readManifest "taxonomy"

writeManifest :: Show manifest => FilePath -> manifest -> Action ()
writeManifest out manifest = do
  let contents = show manifest
  changed <- liftIO $ do
    exists <- doesFileExist out
    if exists
      then (/= T.pack contents) <$> TIO.readFile out
      else pure True
  liftIO $ createDirectoryIfMissing True (takeDirectory out)
  writeFileChanged out contents
  when changed $ liftIO . putStrLn $ "[update/manifest] " <> out

readManifest :: Read manifest => String -> FilePath -> Action manifest
readManifest description path = do
  contents <- readFile' path
  case reads contents of
    [(result, "")] -> pure result
    _ -> fail $ "Invalid " <> description <> " manifest: " <> path
