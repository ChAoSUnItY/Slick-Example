{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Applicative        ((<|>))
import           Control.Monad              (foldM, forM_, when)
import           Config
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Data.List                  (find, nub, sortBy)
import qualified Data.Map.Strict            as Map
import           Data.Time                  (Day, defaultTimeLocale, parseTimeM)
import           Development.Shake
import           Development.Shake.FilePath
import           GHC.Generics               (Generic)
import           Manifest
import           Slick

import qualified Data.Text                  as T

-- | Data shared by the home page and taxonomy listing pages.
data ListingInfo = ListingInfo
    { listingTitle      :: String
    , sitePrefix        :: String
    , posts             :: [ListingPost]
    , tagCloud          :: [TagCount]
    , categoryCloud     :: [TaxonomyLink]
    , showTaxonomy      :: Bool
    , indexPostsPerPage :: Int
    } deriving (Generic, ToJSON)

-- | Data for a blog post.
data Post =
    Post { title   :: String
         , author  :: String
         , content :: String
         , url     :: String
         , date    :: String
         , image   :: Maybe String
         , tags    :: Maybe [String]
         , category :: String
         }
    deriving (Generic, Eq, FromJSON, ToJSON)

-- | A parsed post paired with the Markdown file that produced it.
data LoadedPost = LoadedPost
  { sourcePath :: FilePath
  , postData   :: Post
  }

-- | A taxonomy keeps its human label separate from its URL-safe slug.
data TaxonomyGroup = TaxonomyGroup
  { taxonomyLabel :: String
  , taxonomySlug  :: String
  , taxonomyItems :: [Post]
  }

-- | A link to a neighbouring post. The URL is relative to a page in
-- `docs/posts/`, unlike `Post.url`, which is relative to the site root.
data PostLink =
  PostLink
    { linkTitle :: String
    , linkUrl   :: String
    } deriving (Generic, ToJSON)

type Cache a = () -> Action a

-- Pure post components

-- | A post's tags, defaulting to none.
postTags :: Post -> [String]
postTags = maybe [] id . tags

-- | Parse the date formats used by the starter posts. Posts with an
-- unrecognised date are kept at the end of the index rather than preventing
-- the site from building.
postDate :: Post -> Maybe Day
postDate post =
  foldr (\format result ->
           parseTimeM True defaultTimeLocale format (date post) <|> result) Nothing
    [ "%b %e, %Y"
    , "%e %b %Y"
    , "%Y-%m-%d"
    ]

postLink :: Post -> PostLink
postLink post = PostLink
  { linkTitle = title post
  , linkUrl = dropDirectory1 (url post)
  }

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

neighbours :: Post -> [Post] -> (Maybe Post, Maybe Post)
neighbours target = go Nothing
  where
    go :: Maybe Post -> [Post] -> (Maybe Post, Maybe Post)
    go _ [] = (Nothing, Nothing)
    go newer (current : olderPosts)
      | current == target = (newer, safeHead olderPosts)
      | otherwise = go (Just current) olderPosts

-- Taxonomy components

groupPostsBy
  :: (Post -> [String])
  -> [Post]
  -> Either String (Map.Map String TaxonomyGroup)
groupPostsBy taxonomyValues = foldM addPost Map.empty
  where
    addPost groups post =
      foldM (addValue post) groups (nub $ taxonomyValues post)

    addValue post groups label = do
      slug <- validSlug label
      case Map.lookup slug groups of
        Nothing ->
          Right $ Map.insert slug (TaxonomyGroup label slug [post]) groups
        Just group
          | taxonomyLabel group /= label ->
              Left $ "Taxonomy slug collision: “" <> taxonomyLabel group
                <> "” and “" <> label <> "” both map to “" <> slug <> "”"
          | otherwise ->
              let updatedGroup = group
                    { taxonomyItems = taxonomyItems group ++ [post] }
              in Right $ Map.insert slug updatedGroup groups

    validSlug label =
      let slug = routeSegment label
      in if null slug
           then Left $
             "Taxonomy label has no URL-safe characters: “" <> label <> "”"
           else Right slug

listingPost :: Post -> ListingPost
listingPost post = ListingPost
  { entryTitle = title post
  , entryAuthor = author post
  , listingUrl = url post
  , entryDate = date post
  , entryImage = image post
  , entryTags =
      [ TaxonomyLink tagName
          ("tag" </> routeSegment tagName </> "")
      | tagName <- postTags post
      ]
  , entryCategory = TaxonomyLink (category post)
      ("category" </> routeSegment (category post) </> "")
  }

buildTagCloud :: [Post] -> [TagCount]
buildTagCloud allPosts =
  let tagCounts = Map.toAscList . Map.fromListWith (+) $
        [ (tagName, 1 :: Int)
        | post <- allPosts
        , tagName <- postTags post
        ]
      allCounts = map snd tagCounts
      maxCount = maximum (1 : allCounts)
      minCount = minimum (maxCount : allCounts)
      weightFor n
        | maxCount == minCount = 3
        | otherwise =
            1 + round (4 * fromIntegral (n - minCount)
                         / fromIntegral (maxCount - minCount) :: Double)
      toTagCount (tagName, n) =
        TagCount tagName n (weightFor n)
          ("tag" </> routeSegment tagName </> "")
  in map toTagCount tagCounts

buildCategoryCloud :: [Post] -> [TaxonomyLink]
buildCategoryCloud allPosts =
  [ TaxonomyLink categoryName
      ("category" </> routeSegment categoryName </> "")
  | categoryName <- Map.keys . Map.fromList $
      [ (category post, ()) | post <- allPosts ]
  ]

-- Shake actions

announce :: String -> FilePath -> Action ()
announce ruleName target =
  liftIO . putStrLn $ "[" <> ruleName <> "] " <> target

-- | Load and process a post's Markdown and frontmatter.
loadPost :: FilePath -> Action Post
loadPost srcPath = do
  announce "load/post" srcPath
  postContent <- readFile' srcPath
  -- Load post content and metadata as a JSON blob.
  parsedData <- markdownToHTML . T.pack $ postContent
  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl
  convert . withPostUrl $ parsedData

-- | Load and order posts once. The `getDirectoryFiles` call is tracked by
-- Shake, so adding or removing a source invalidates this collection too.
loadPosts :: Action [LoadedPost]
loadPosts = do
  paths <- getDirectoryFiles "." [siteFolder </> "posts//*.md"]
  posts' <- mapM loadPost paths
  let loadedPosts = zipWith LoadedPost paths posts'
  return $ sortBy (\a b -> compare (postDate $ postData b) (postDate $ postData a)) loadedPosts

requiredPosts :: Cache [LoadedPost] -> Action [Post]
requiredPosts getPosts = do
  loadedPosts <- getPosts ()
  need $ map sourcePath loadedPosts
  pure $ map postData loadedPosts

writeListing
  :: FilePath
  -> HomeManifest
  -> Action ()
writeListing destination manifest =
  let listingInfo = ListingInfo
        { listingTitle = siteTitle
        , sitePrefix = ""
        , posts = homePosts manifest
        , tagCloud = homeTagCloud manifest
        , categoryCloud = homeCategoryCloud manifest
        , showTaxonomy = True
        , indexPostsPerPage = postsPerIndexPage
        }
  in renderListing destination listingInfo

writeTaxonomyListing
  :: FilePath
  -> FilePath
  -> String
  -> [ListingPost]
  -> Action ()
writeTaxonomyListing destination pathPrefix heading listingPosts =
  let listingInfo = ListingInfo
        { listingTitle = heading
        , sitePrefix = pathPrefix
        , posts = listingPosts
        , tagCloud = []
        , categoryCloud = []
        , showTaxonomy = False
        , indexPostsPerPage = postsPerIndexPage
        }
  in renderListing destination listingInfo

renderListing :: FilePath -> ListingInfo -> Action ()
renderListing destination listingInfo = do
  need [indexTemplate]
  listingT <- compileTemplate' indexTemplate
  writeFileChanged destination . T.unpack $
    substitute listingT (toJSON listingInfo)

-- | Render a post with links to its neighbours: next is newer and previous is
-- older, relative to the newest-first index order.
writePost :: Post -> Maybe Post -> Maybe Post -> Action ()
writePost post previousPost nextPost = do
  need [postTemplate]
  template <- compileTemplate' postTemplate
  let tagLinks =
        [ TaxonomyLink tagName
            ("../tag" </> routeSegment tagName </> "")
        | tagName <- postTags post
        ]
      categoryLink = TaxonomyLink (category post)
        ("../category" </> routeSegment (category post) </> "")
      postWithTaxonomy = toJSON post
        & _Object . at "tags" ?~ toJSON tagLinks
        & _Object . at "hasTags" ?~ toJSON (not $ null tagLinks)
        & _Object . at "category" ?~ toJSON categoryLink
      previousLink = maybe Null (toJSON . postLink) previousPost
      nextLink = maybe Null (toJSON . postLink) nextPost
      postWithNavigation = postWithTaxonomy
        & _Object . at "previousPost" ?~ previousLink
        & _Object . at "nextPost" ?~ nextLink
      postHTML = T.unpack $ substitute template postWithNavigation
  writeFileChanged (outputFolder </> url post) postHTML

copyStatic :: FilePath -> Action ()
copyStatic out = do
  announce "copy/asset" out
  let source = staticSourceForOutput out
  need [source]
  copyFileChanged source out

removeStaleOwnedOutputs :: [FilePath] -> [FilePath] -> Action ()
removeStaleOwnedOutputs validOutputs validManifests = do
  removeStaleFiles outputFolder
    [ "posts//*.html"
    , "tag/*/index.html"
    , "category/*/index.html"
    , "css//*"
    , "images//*"
    , "js//*"
    ]
    validOutputs

  removeStaleFiles listingManifestRoot
    [ "*.posts"
    , "tag/*.posts"
    , "category/*.posts"
    ]
    validManifests

removeStaleFiles :: FilePath -> [FilePattern] -> [FilePath] -> Action ()
removeStaleFiles root ownedPatterns validPaths = do
  existingPaths <- getDirectoryFiles root ownedPatterns
  let stalePaths = filter (`notElem` validPaths) existingPaths
  forM_ stalePaths $ \path ->
    announce "remove/stale" (root </> path)
  removeFilesAfter root stalePaths

buildSite
  :: Cache [LoadedPost]
  -> Cache (Map.Map String TaxonomyGroup)
  -> Cache (Map.Map String TaxonomyGroup)
  -> Action ()
buildSite getPosts getTags getCategories = do
  announce "build" outputFolder

  loadedPosts <- getPosts ()
  need $ map sourcePath loadedPosts
  availableTags <- getTags ()
  availableCategories <- getCategories ()
  staticFiles <- getDirectoryFiles "."
    [ siteFolder </> "images//*"
    , siteFolder </> "css//*"
    , siteFolder </> "js//*"
    ]

  let postPages =
        map (makeRelative outputFolder . postOutput . sourcePath) loadedPosts
      tagPages = map tagPagePath $ Map.keys availableTags
      categoryPages = map categoryPagePath $ Map.keys availableCategories
      staticPages =
        map (makeRelative outputFolder . staticOutput) staticFiles
      validOutputs =
        postPages ++ tagPages ++ categoryPages ++ staticPages

      manifestPaths =
           [homeManifest]
        ++ map tagManifest (Map.keys availableTags)
        ++ map categoryManifest (Map.keys availableCategories)
      validManifests =
        map (makeRelative listingManifestRoot) manifestPaths

  need $
       [outputFolder </> "index.html"]
    ++ map (outputFolder </>) validOutputs
    ++ manifestPaths

  removeStaleOwnedOutputs validOutputs validManifests

-- Rules

-- The building process is as follow:
-- 
-- Loads all posts and transform to data
--                  |
-- Compute necessary datas and generates
--             as manifest
--                  |
-- output html based on manifest changes
--                  |
-- use `serve` or other web deployer to
--        deploy production site

buildRules :: Rules ()
buildRules = do
  want ["build"]

  -- Caches
  getPosts <- newCache $ const loadPosts

  getTags <- newCache $ const $ do
    allPosts <- requiredPosts getPosts
    either fail pure $ groupPostsBy postTags allPosts

  getCategories <- newCache $ const $ do
    allPosts <- requiredPosts getPosts
    either fail pure $ groupPostsBy (\post -> [category post]) allPosts

  -- home index manifest
  homeManifest %> \out -> do
    allPosts <- requiredPosts getPosts
    writeHomeManifest out
      (map listingPost allPosts)
      (buildTagCloud allPosts)
      (buildCategoryCloud allPosts)

  -- post manifests
  listingManifestRoot </> "tag/*.posts" %> \out -> do
    availableTags <- getTags ()
    let tag' = dropExtension $ takeFileName out
    case Map.lookup tag' availableTags of
      Nothing -> fail $ "No tag source for " <> out
      Just group -> writeTaxonomyManifest out
        (taxonomyLabel group)
        (taxonomySlug group)
        (map listingPost $ taxonomyItems group)

  -- category manifest
  listingManifestRoot </> "category/*.posts" %> \out -> do
    availableCategories <- getCategories ()
    let category' = dropExtension $ takeFileName out
    case Map.lookup category' availableCategories of
      Nothing -> fail $ "No category source for " <> out
      Just group -> writeTaxonomyManifest out
        (taxonomyLabel group)
        (taxonomySlug group)
        (map listingPost $ taxonomyItems group)

  -- home index html
  outputFolder </> "index.html" %> \out -> do
    announce "render/index" out
    manifest <- readHomeManifest
    writeListing out manifest

  -- post html
  outputFolder </> "posts//*.html" %> \out -> do
    announce "render/post" out
    let source = postSourceForOutput out
    need [source]
    allPosts <- requiredPosts getPosts
    case find ((== dropDirectory1 out) . url) allPosts of
      Nothing -> fail $ "No post source for " <> out
      Just post -> do
        let (newer, older) = neighbours post allPosts
        writePost post older newer

  -- tag html
  outputFolder </> "tag/*/index.html" %> \out -> do
    announce "render/tag" out
    let slug = takeFileName $ takeDirectory out
    manifest <- readTaxonomyManifest $ tagManifest slug
    when (manifestSlug manifest /= slug) $
      fail $ "Tag manifest slug mismatch for " <> out
    writeTaxonomyListing out "../../"
      ("Posts tagged “" <> manifestLabel manifest <> "”")
      (manifestPosts manifest)

  -- category html
  outputFolder </> "category/*/index.html" %> \out -> do
    announce "render/category" out
    let slug = takeFileName $ takeDirectory out
    manifest <- readTaxonomyManifest $ categoryManifest slug
    when (manifestSlug manifest /= slug) $
      fail $ "Category manifest slug mismatch for " <> out
    writeTaxonomyListing out "../../"
      ("Posts in “" <> manifestLabel manifest <> "”")
      (manifestPosts manifest)

  outputFolder </> "images//*" %> copyStatic
  outputFolder </> "css//*"    %> copyStatic
  outputFolder </> "js//*"     %> copyStatic

  phony "build" $ buildSite getPosts getTags getCategories

main :: IO ()
main = shakeArgs buildOptions buildRules
