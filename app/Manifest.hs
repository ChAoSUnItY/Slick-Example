{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Manifest is responsible for collecting post metadata and construct
-- | a minimal changes to update output html files.
module Manifest
  ( HomeManifest(..)
  , ListingPost(..)
  , CloudItem(..)
  , PostLink(..)
  , PostNavigation(..)
  , TaxonomyLink(..)
  , TaxonomyManifest(..)
  , categoryManifest
  , homeManifest
  , listingManifestRoot
  , postNavigationManifest
  , postNavigationRoot
  , readHomeManifest
  , readPostNavigationManifest
  , readTaxonomyManifest
  , tagManifest
  , writeHomeManifest
  , writePostNavigationManifest
  , writeTaxonomyManifest
  ) where

import           Control.Monad              (when)
import           Data.Aeson                 (ToJSON)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Development.Shake         (Action, liftIO, readFile',
                                            writeFileChanged)
import           Development.Shake.FilePath ((</>), (<.>), (-<.>),
                                             dropDirectory1, takeDirectory)
import           GHC.Generics               (Generic)
import           System.Directory           (createDirectoryIfMissing,
                                            doesFileExist)

-- | A weighted tag or category entry in the home-page clouds.
data CloudItem = CloudItem
  { cloudName   :: String
  , cloudCount  :: Int
  , cloudWeight :: Int
  , cloudUrl    :: String
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

-- | Links on a post page are relative to the posts directory.
data PostLink = PostLink
  { linkTitle :: String
  , linkUrl   :: String
  } deriving (Generic, Read, Show, ToJSON)

data PostNavigation = PostNavigation
  { navigationPrevious :: Maybe PostLink
  , navigationNext     :: Maybe PostLink
  } deriving (Read, Show)

data HomeManifest = HomeManifest
  { homePosts         :: [ListingPost]
  , homeTagCloud      :: [CloudItem]
  , homeCategoryCloud :: [CloudItem]
  } deriving (Read, Show)

data TaxonomyManifest = TaxonomyManifest
  { manifestLabel :: String
  , manifestSlug  :: String
  , manifestPosts :: [ListingPost]
  } deriving (Read, Show)

listingManifestRoot :: FilePath
listingManifestRoot = "_build/listings"

postNavigationRoot :: FilePath
postNavigationRoot = "_build/navigation"

postNavigationManifest :: FilePath -> FilePath
postNavigationManifest postUrl =
  postNavigationRoot </> dropDirectory1 (postUrl -<.> "nav")

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
  -> [CloudItem]
  -> [CloudItem]
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

writePostNavigationManifest :: FilePath -> PostNavigation -> Action ()
writePostNavigationManifest = writeManifest

readPostNavigationManifest :: FilePath -> Action PostNavigation
readPostNavigationManifest = readManifest "post navigation"

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
