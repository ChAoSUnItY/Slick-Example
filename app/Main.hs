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
import           Data.Char                  (isDigit)
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
    , metaTitle         :: String
    , metaDescription   :: String
    , sitePrefix        :: String
    , posts             :: [ListingPost]
    , tagCloud          :: [CloudItem]
    , categoryCloud     :: [CloudItem]
    , showTaxonomy      :: Bool
    , hasPagination    :: Bool
    , pageNumber       :: Int
    , pageCount        :: Int
    , previousPage     :: Maybe PageLink
    , nextPage         :: Maybe PageLink
    } deriving (Generic, ToJSON)

data PageLink = PageLink
    { pageHref :: FilePath
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
          ("tag" </> routeSegment tagName </> homePagePath 1)
      | tagName <- postTags post
      ]
  , entryCategory = TaxonomyLink (category post)
      ("category" </> routeSegment (category post) </> homePagePath 1)
  }

buildWeightedCloud :: FilePath -> [String] -> [CloudItem]
buildWeightedCloud taxonomy labels =
  case Map.toAscList $ Map.fromListWith (+)
         [ (label, 1 :: Int) | label <- labels ] of
    [] -> []
    counts ->
      let frequencies = map snd counts
          minimumCount = minimum frequencies
          spread = maximum frequencies - minimumCount
          weight n
            | spread == 0 = 3
            | otherwise = 1 + round
                (4 * fromIntegral (n - minimumCount) / fromIntegral spread :: Double)
      in [ CloudItem label n (weight n)
             (taxonomy </> routeSegment label </> homePagePath 1)
         | (label, n) <- counts
         ]

buildTagCloud :: [Post] -> [CloudItem]
buildTagCloud allPosts = buildWeightedCloud "tag"
  [ tagName | post <- allPosts, tagName <- nub $ postTags post ]

buildCategoryCloud :: [Post] -> [CloudItem]
buildCategoryCloud allPosts = buildWeightedCloud "category" $ map category allPosts

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

pageCountFor :: [a] -> Int
pageCountFor items
  | postsPerIndexPage <= 0 = error "postsPerIndexPage must be positive"
  | otherwise = max 1 $ (length items + postsPerIndexPage - 1) `div` postsPerIndexPage

pageNumbers :: [a] -> [Int]
pageNumbers items = [1 .. pageCountFor items]

pageSlice :: Int -> [a] -> [a]
pageSlice page = take postsPerIndexPage . drop ((page - 1) * postsPerIndexPage)

pageFromOutput :: FilePath -> Action Int
pageFromOutput out =
  let name = dropExtension $ takeFileName out
  in case reads name of
       [(page, "")] | page >= 1 && all isDigit name && show page == name -> pure page
       _ -> fail $ "Invalid listing page: " <> out

listingNavigation :: Int -> Int -> ListingInfo -> ListingInfo
listingNavigation page total info = info
  { posts = pageSlice page (posts info)
  , metaTitle = metaTitle info <> pageTitleSuffix
  , metaDescription = metaDescription info <> pageDescriptionSuffix
  , hasPagination = total > 1
  , pageNumber = page
  , pageCount = total
  , previousPage = if page > 1 then Just $ PageLink (homePagePath $ page - 1) else Nothing
  , nextPage = if page < total then Just $ PageLink (homePagePath $ page + 1) else Nothing
  }
  where
    pageTitleSuffix = if page == 1 then "" else " — Page " <> show page
    pageDescriptionSuffix =
      if page == 1 then "" else " Page " <> show page <> " of " <> show total <> "."

writeListing :: FilePath -> Int -> HomeManifest -> Action ()
writeListing destination page manifest =
  let listingInfo = ListingInfo
        { listingTitle = siteTitle
        , metaTitle = siteTitle <> " — a Slick blog"
        , metaDescription = "Notes on code, tools, and things worth writing down."
        , sitePrefix = ""
        , posts = homePosts manifest
        , tagCloud = homeTagCloud manifest
        , categoryCloud = homeCategoryCloud manifest
        , showTaxonomy = True
        , hasPagination = False
        , pageNumber = 1
        , pageCount = 1
        , previousPage = Nothing
        , nextPage = Nothing
        }
      total = pageCountFor $ homePosts manifest
  in if page > total then fail $ "No home page " <> show page
     else renderListing destination $ listingNavigation page total listingInfo

writeTaxonomyListing
  :: FilePath
  -> FilePath
  -> String
  -> Int
  -> [ListingPost]
  -> Action ()
writeTaxonomyListing destination pathPrefix heading page listingPosts =
  let listingInfo = ListingInfo
        { listingTitle = heading
        , metaTitle = heading <> " — " <> siteTitle
        , metaDescription = heading <> " on " <> siteTitle <> "."
        , sitePrefix = pathPrefix
        , posts = listingPosts
        , tagCloud = []
        , categoryCloud = []
        , showTaxonomy = False
        , hasPagination = False
        , pageNumber = 1
        , pageCount = 1
        , previousPage = Nothing
        , nextPage = Nothing
        }
      total = pageCountFor listingPosts
  in if page > total then fail $ "No taxonomy page " <> show page <> ": " <> destination
     else renderListing destination $ listingNavigation page total listingInfo

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
            ("../tag" </> routeSegment tagName </> homePagePath 1)
        | tagName <- postTags post
        ]
      categoryLink = TaxonomyLink (category post)
        ("../category" </> routeSegment (category post) </> homePagePath 1)
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

copyFirstPageToIndex :: FilePath -> Action ()
copyFirstPageToIndex out = do
  let firstPage = takeDirectory out </> homePagePath 1
  need [firstPage]
  copyFileChanged firstPage out

removeStaleOwnedOutputs :: [FilePath] -> [FilePath] -> Action ()
removeStaleOwnedOutputs validOutputs validManifests = do
  -- Remove stale docs files
  removeStaleFiles outputFolder
    [ "posts//*.html"
    , "tag/*/*.html"
    , "category/*/*.html"
    , "css//*"
    , "images//*"
    , "js//*"
    ]
    validOutputs

  rootHtml <- getDirectoryFiles outputFolder ["*.html"]
  let numberedPages = filter (all isDigit . dropExtension . takeFileName) rootHtml
      stalePages = filter (`notElem` validOutputs) numberedPages
  forM_ stalePages $ \path -> announce "remove/stale" (outputFolder </> path)
  removeFilesAfter outputFolder stalePages

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

  let manifestPaths =
           [homeManifest]
        ++ map tagManifest (Map.keys availableTags)
        ++ map categoryManifest (Map.keys availableCategories)
      validManifests =
        map (makeRelative listingManifestRoot) manifestPaths

  need manifestPaths
  homeListing <- readHomeManifest
  tagListings <- mapM (readTaxonomyManifest . tagManifest) (Map.keys availableTags)
  categoryListings <- mapM (readTaxonomyManifest . categoryManifest) (Map.keys availableCategories)

  -- Computes all necessary output files to Shake file dependency
  let postPages =
        map (makeRelative outputFolder . postOutput . sourcePath) loadedPosts
      homePages = "index.html" : map homePagePath (pageNumbers $ homePosts homeListing)
      tagPages = concatMap (\manifest ->
        ("tag" </> manifestSlug manifest </> "index.html") :
        map (tagPagePath $ manifestSlug manifest) (pageNumbers $ manifestPosts manifest)) tagListings
      categoryPages = concatMap (\manifest ->
        ("category" </> manifestSlug manifest </> "index.html") :
        map (categoryPagePath $ manifestSlug manifest) (pageNumbers $ manifestPosts manifest)) categoryListings
      staticPages =
        map (makeRelative outputFolder . staticOutput) staticFiles
      validOutputs =
        homePages ++ postPages ++ tagPages ++ categoryPages ++ staticPages

  need $ map (outputFolder </>) validOutputs

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

  -- The index aliases are exact copies of page one.
  outputFolder </> "*.html" %> \out -> do
    announce "render/index" out
    if takeFileName out == "index.html"
      then do
        copyFirstPageToIndex out
      else do
        page <- pageFromOutput out
        manifest <- readHomeManifest
        writeListing out page manifest

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
  outputFolder </> "tag/*/*.html" %> \out -> do
    announce "render/tag" out
    if takeFileName out == "index.html"
      then do
        copyFirstPageToIndex out
      else do
        page <- pageFromOutput out
        let slug = takeFileName $ takeDirectory out
        manifest <- readTaxonomyManifest $ tagManifest slug
        when (manifestSlug manifest /= slug) $
          fail $ "Tag manifest slug mismatch for " <> out
        writeTaxonomyListing out "../../"
          ("Posts tagged “" <> manifestLabel manifest <> "”") page
          (manifestPosts manifest)

  -- category html
  outputFolder </> "category/*/*.html" %> \out -> do
    announce "render/category" out
    if takeFileName out == "index.html"
      then do
        copyFirstPageToIndex out
      else do
        page <- pageFromOutput out
        let slug = takeFileName $ takeDirectory out
        manifest <- readTaxonomyManifest $ categoryManifest slug
        when (manifestSlug manifest /= slug) $
          fail $ "Category manifest slug mismatch for " <> out
        writeTaxonomyListing out "../../"
          ("Posts in “" <> manifestLabel manifest <> "”") page
          (manifestPosts manifest)

  outputFolder </> "images//*" %> copyStatic
  outputFolder </> "css//*"    %> copyStatic
  outputFolder </> "js//*"     %> copyStatic

  phony "build" $ buildSite getPosts getTags getCategories

main :: IO ()
main = shakeArgs buildOptions buildRules
