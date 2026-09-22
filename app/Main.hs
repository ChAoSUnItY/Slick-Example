{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Applicative        ((<|>))
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Data.Char                  (isAlphaNum, toLower)
import           Data.List                  (find, intercalate, sortBy)
import qualified Data.Map.Strict            as Map
import           Data.Time                  (Day, defaultTimeLocale, parseTimeM)
import           Development.Shake
import           Development.Shake.Classes (Binary)
import           Development.Shake.FilePath
import           GHC.Generics               (Generic)
import           Slick

import qualified Data.Text                  as T
import Control.Monad (forM_)

outputFolder :: FilePath
outputFolder = "docs/"

-- | Maximum number of posts rendered on one index page.
postsPerIndexPage :: Int
postsPerIndexPage = 5

-- | A single entry in the tag cloud. `weight` is a 1-5 bucket, sized
-- relative to the most-used tag, so the template can pick a font size
-- purely from data (via a `tag-size-N` CSS class) without doing any
-- math itself.
data TagCount =
  TagCount
    { tag    :: String
    , count  :: Int
    , weight :: Int
    , tagUrl :: String
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | A tag or category link rendered from a particular page depth.
data TaxonomyLink = TaxonomyLink
  { taxonomyName :: String
  , taxonomyUrl  :: String
  } deriving (Generic, Show, ToJSON)

-- | A post as it appears in a listing page. Its links are made relative to
-- the page being rendered, so the same template works at /, /tag/*, and
-- /category/*.
data ListingPost = ListingPost
  { entryTitle  :: String
  , entryAuthor :: String
  , listingUrl :: String
  , entryDate   :: String
  , entryImage  :: Maybe String
  , entryTags   :: [TaxonomyLink]
  , entryCategory :: TaxonomyLink
  } deriving (Generic, Show, ToJSON)

-- | Data shared by the home page and taxonomy listing pages.
data ListingInfo = ListingInfo
    { listingTitle      :: String
    , sitePrefix        :: String
    , posts             :: [ListingPost]
    , tagCloud          :: [TagCount]
    , categoryCloud     :: [TaxonomyLink]
    , showTaxonomy      :: Bool
    , indexPostsPerPage :: Int
    } deriving (Generic, Show, ToJSON)

-- | Data for a blog post.
-- `tags` is optional (`Maybe [String]`) so posts written before you
-- adopted tags -- or that simply don't have any -- still parse fine;
-- aeson's generic decoder treats a missing/omitted "tags" key in the
-- frontmatter as `Nothing`, same as it already does for `image`.
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
    deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

-- | A link to a neighbouring post. The URL is relative to a page in
-- `docs/posts/`, unlike `Post.url`, which is relative to the site root.
data PostLink =
  PostLink
    { linkTitle :: String
    , linkUrl   :: String
    } deriving (Generic, Show, ToJSON)

-- | A post's tags, defaulting to none.
postTags :: Post -> [String]
postTags = maybe [] id . tags

groupPostsBy :: (Post -> [String]) -> [Post] -> Map.Map String [Post]
groupPostsBy vals =
  Map.fromListWith (flip (++)) . concatMap entries
  where
    entries post = [ (value, [post]) | value <- vals post ]

-- | Build a tag cloud from every post's tags: alphabetical and deduped,
-- with a 1-5 "weight" bucket sized relative to the most-used tag.
buildTagCloud :: [Post] -> [TagCount]
buildTagCloud = buildTagCloudAt ""

buildTagCloudAt :: FilePath -> [Post] -> [TagCount]
buildTagCloudAt pathPrefix ps =
  let counts   = Map.toAscList . Map.fromListWith (+) $
                   [ (t, 1 :: Int) | p <- ps, t <- postTags p ]
      allCounts = map snd counts
      maxCount  = maximum (1 : allCounts)
      minCount  = minimum (maxCount : allCounts)
      bucket n
        | maxCount == minCount = 3
        | otherwise =
            1 + round (4 * fromIntegral (n - minCount)
                         / fromIntegral (maxCount - minCount) :: Double)
  in [ TagCount t n (bucket n) (pathPrefix </> "tag" </> routeSegment t </> "")
     | (t, n) <- counts
     ]

buildCategoryCloudAt :: FilePath -> [Post] -> [TaxonomyLink]
buildCategoryCloudAt pathPrefix ps =
  [ TaxonomyLink c (pathPrefix </> "category" </> routeSegment c </> "")
  | c <- Map.keys . Map.fromList $ [ (category p, ()) | p <- ps ]
  ]

routeSegment :: String -> String
routeSegment = intercalate "-" . words . map toSafeChar
  where
    toSafeChar c | isAlphaNum c = toLower c
                 | otherwise    = ' '

listingPost :: FilePath -> Post -> ListingPost
listingPost pathPrefix post = ListingPost
  { entryTitle = title post
  , entryAuthor = author post
  , listingUrl = pathPrefix </> url post
  , entryDate = date post
  , entryImage = fmap (pathPrefix </>) (image post)
  , entryTags = [ TaxonomyLink t (pathPrefix </> "tag" </> routeSegment t </> "")
           | t <- postTags post
           ]
  , entryCategory = TaxonomyLink (category post)
      (pathPrefix </> "category" </> routeSegment (category post) </> "")
  }

writeListing :: FilePath -> FilePath -> String -> Bool -> [Post] -> [Post] -> Action ()
writeListing destination pathPrefix heading includeTaxonomy allPosts listingPosts = do
  listingT <- compileTemplate' "site/templates/index.html"
  let listingInfo = ListingInfo
        { listingTitle = heading
        , sitePrefix = pathPrefix
        , posts = map (listingPost pathPrefix) listingPosts
        , tagCloud = buildTagCloudAt pathPrefix allPosts
        , categoryCloud = buildCategoryCloudAt pathPrefix allPosts
        , showTaxonomy = includeTaxonomy
        , indexPostsPerPage = postsPerIndexPage
        }
  writeFileChanged destination . T.unpack $
    substitute listingT (toJSON listingInfo)

-- | Parse the date formats used by the starter posts.  Posts with an
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

-- | Newest posts first; preserve the relative order of posts whose dates are
-- identical or cannot be parsed.
sortPostsByDate :: [Post] -> [Post]
sortPostsByDate = sortBy $ \a b -> compare (postDate b) (postDate a)

-- | Load and order posts once. The `getDirectoryFiles` call is tracked by
-- Shake, so adding or removing a source invalidates this collection too.
loadPosts :: Action [(FilePath, Post)]
loadPosts = do
  paths <- getDirectoryFiles "." ["site/posts//*.md"]
  posts' <- mapM loadPost paths
  return $
    sortBy
      (\(_, a) (_, b) -> compare (postDate b) (postDate a))
      (zip paths posts')

-- | Load and process a post's Markdown and frontmatter.
loadPost :: FilePath -> Action Post
loadPost srcPath = do
  announce "load/post" srcPath
  postContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  postData <- markdownToHTML . T.pack $ postContent
  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl
  convert . withPostUrl $ postData

-- | Render a post with links to its neighbours: next is newer and previous is
-- older, relative to the newest-first index order.
writePost :: Post -> Maybe Post -> Maybe Post -> Action ()
writePost post previousPost nextPost = do
  template <- compileTemplate' "site/templates/post.html"
  let postData = toJSON post
        & _Object . at "tags" ?~ toJSON
            [ TaxonomyLink t ("../tag" </> routeSegment t </> "") | t <- postTags post ]
        & _Object . at "category" ?~ toJSON
            (TaxonomyLink (category post)
              ("../category" </> routeSegment (category post) </> ""))
      withNavigation = postData
        & _Object . at "previousPost" ?~ maybe Null (toJSON . postLink) previousPost
        & _Object . at "nextPost" ?~ maybe Null (toJSON . postLink) nextPost
      postHTML = T.unpack $ substitute template withNavigation
  writeFile' (outputFolder </> url post) postHTML

postLink :: Post -> PostLink
postLink post = PostLink
  { linkTitle = title post
  , linkUrl = dropDirectory1 (url post)
  }

templateFiles :: Action ()
templateFiles = getDirectoryFiles "." ["site/templates//*.html"] >>= need

postOutput :: FilePath -> FilePath
postOutput source = outputFolder </> dropDirectory1 (source -<.> "html")

staticOutput :: FilePath -> FilePath
staticOutput source = outputFolder </> dropDirectory1 source

announce :: String -> FilePath -> Action ()
announce ruleName target =
  liftIO . putStrLn $ "[" <> ruleName <> "] " <> target

buildRules :: Rules ()
buildRules = do
  want ["build"]
  getPosts <- newCache $ const loadPosts

  -- Let rule requires all posts, and returns
  -- all loaded posts
  let requiredPosts = do
        loaded <- getPosts ()
        need $ map fst loaded
        pure $ map snd loaded

  getTags <- newCache $ const $
    groupPostsBy (map routeSegment . postTags) <$> requiredPosts

  getCategories <- newCache $ const $
    groupPostsBy (\post -> [routeSegment $ category post]) <$> requiredPosts

  phony "build" $ do
    announce "build" "doc"
    availableTags <- getTags ()
    availableCategories <- getCategories ()
    let tagPages =
            [ "tag" </> tagName </> "index.html"
            | (tagName, _) <- Map.toAscList availableTags
            ]

        categoryPages =
            [ "category" </> categoryName </> "index.html"
            | (categoryName, _) <- Map.toAscList availableCategories
            ]

        validTaxonomyPages = tagPages ++ categoryPages

    staticFiles <- getDirectoryFiles "." ["site/images//*", "site/css//*", "site/js//*"]


    -- Need all static files
    need $ map staticOutput staticFiles
    -- Need all post files
    posts' <- getPosts ()
    forM_ posts' (\(loadedPath, _) -> do
      need $ [loadedPath, postOutput loadedPath]
      )
    -- Need all taxonomy pages
    need $ map (outputFolder </>) validTaxonomyPages
    -- Finally, an index page for whole site
    need [ "docs/index.html" ]

    -- Computes stale taxonomy pages and remove it after site building
    existingTaxonomyPages <- getDirectoryFiles outputFolder
        [ "tag/*/index.html"
        , "category/*/index.html"
        ]

    let staleTaxonomyPages =
            filter (`notElem` validTaxonomyPages) existingTaxonomyPages

    forM_ staleTaxonomyPages $ \stalePage ->
      announce "remove/stale" (outputFolder </> stalePage)

    removeFilesAfter outputFolder staleTaxonomyPages

  "docs/index.html" %> \out -> do
    announce "render/index" out
    templateFiles
    allPosts <- requiredPosts
    writeListing out "" "Field Notes" True allPosts allPosts

  "docs/posts//*.html" %> \out -> do
    announce "render/post" out
    templateFiles
    let source = ("site/posts" </> makeRelative (outputFolder </> "posts") out) -<.> "md"
    need [source]
    allPosts <- requiredPosts
    case find ((== dropDirectory1 out) . url) allPosts of
      Nothing -> fail $ "No post source for " <> out
      Just post -> do
        let (newer, older) = neighbours post allPosts
        writePost post older newer

  "docs/tag/*/index.html" %> \out -> do
    announce "render/tag" out
    templateFiles
    allPosts <- requiredPosts
    availableTags <- getTags ()
    let tagName = takeFileName . takeDirectory $ out
    case Map.lookup tagName availableTags of
      Nothing -> fail $ "No tag source for " <> out
      Just posts' -> do
        writeListing out "../../" ("Posts tagged “" <> tagName <> "”")
          False allPosts posts'

  "docs/category/*/index.html" %> \out -> do
    announce "render/category" out
    templateFiles
    allPosts <- requiredPosts
    availableCategories <- getCategories ()
    let catName = takeFileName . takeDirectory $ out
    case Map.lookup catName availableCategories of
      Nothing -> fail $ "No category source for " <> out
      Just posts' ->
        writeListing out "../../" ("Posts in “" <> catName <> "”")
          False allPosts posts'

  "docs/images//*" %> copyStatic
  "docs/css//*"    %> copyStatic
  "docs/js//*"     %> copyStatic
  where
    copyStatic :: FilePath -> Action ()
    copyStatic out = do
      announce "copy asset" out
      let source = "site" </> makeRelative outputFolder out
      need [source]
      copyFileChanged source out

    neighbours :: Post -> [Post] -> (Maybe Post, Maybe Post)
    neighbours post posts' =
      case dropWhile ((/= post) . snd) $ zip (Nothing : map Just posts') posts' of
        ((newer, _) : rest) -> (newer, fmap snd (safeHead rest))
        []                 -> (Nothing, Nothing)
      where
        safeHead []    = Nothing
        safeHead (x:_) = Just x

main :: IO ()
main = shakeArgs
    shakeOptions { shakeLintInside = ["."] }
    buildRules
