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

-- | Source paths and their parsed posts travel together through the build.
-- `orderedPosts` is newest-first, which is also the order wanted by every
-- listing and by post navigation.
data LoadedPosts = LoadedPosts
  { postFiles    :: [FilePath]
  , orderedPosts :: [Post]
  }

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
  writeFile' destination . T.unpack $
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
loadPosts :: Action LoadedPosts
loadPosts = do
  files <- getDirectoryFiles "." ["site/posts//*.md"]
  LoadedPosts files . sortPostsByDate <$> mapM loadPost files

-- | Load and process a post's Markdown and frontmatter.
loadPost :: FilePath -> Action Post
loadPost srcPath = do
  announce "load source" srcPath
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

-- | Direct stdout messages make it easy to distinguish planning work from
-- rules that actually regenerated an output file.
announce :: String -> FilePath -> Action ()
announce ruleName target =
  liftIO . putStrLn $ "[" <> ruleName <> "] " <> target

-- | Specific build rules for the Shake system.  Every file in docs is now an
-- explicit target: Shake can consequently skip targets whose inputs have not
-- changed, instead of running the complete renderer for every invocation.
buildRules :: Rules ()
buildRules = do
  want ["site"]
  getPosts <- newCache (\() -> loadPosts)
  let trackedPosts = do
        loaded <- getPosts ()
        need (postFiles loaded)
        pure (orderedPosts loaded)

  phony "site" $ do
    announce "phony" "site"
    loaded <- getPosts ()
    need (postFiles loaded)
    staticFiles <- getDirectoryFiles "." ["site/images//*", "site/css//*", "site/js//*"]
    need $ "docs/index.html"
         : map postOutput (postFiles loaded)
        ++ map staticOutput staticFiles
        ++ [ outputFolder </> "tag" </> routeSegment t </> "index.html"
           | p <- orderedPosts loaded, t <- postTags p
           ]
        ++ [ outputFolder </> "category" </> routeSegment (category p) </> "index.html"
           | p <- orderedPosts loaded
           ]

  "docs/index.html" %> \out -> do
    announce "render index" out
    templateFiles
    allPosts <- trackedPosts
    writeListing out "" "Field Notes" True allPosts allPosts

  "docs/posts//*.html" %> \out -> do
    announce "render post" out
    templateFiles
    let source = ("site/posts" </> makeRelative (outputFolder </> "posts") out) -<.> "md"
    need [source]
    allPosts <- trackedPosts
    case find ((== dropDirectory1 out) . url) allPosts of
      Nothing -> fail $ "No post source for " <> out
      Just post -> do
        let (newer, older) = neighbours post allPosts
        writePost post older newer

  "docs/tag/*/index.html" %> \out -> do
    announce "render tag" out
    templateFiles
    allPosts <- trackedPosts
    let segment = takeFileName . takeDirectory $ out
        matchingTags = [ t | p <- allPosts, t <- postTags p, routeSegment t == segment ]
    case matchingTags of
      [] -> fail $ "No tag source for " <> out
      (tagName:_) -> writeListing out "../../" ("Posts tagged “" <> tagName <> "”")
                       False allPosts (filter (elem tagName . postTags) allPosts)

  "docs/category/*/index.html" %> \out -> do
    announce "render category" out
    templateFiles
    allPosts <- trackedPosts
    let segment = takeFileName . takeDirectory $ out
        matchingCategories = [ category p | p <- allPosts, routeSegment (category p) == segment ]
    case matchingCategories of
      [] -> fail $ "No category source for " <> out
      (categoryName:_) -> writeListing out "../../" ("Posts in “" <> categoryName <> "”")
                            False allPosts (filter ((== categoryName) . category) allPosts)

  "docs/images//*" %> copyStatic
  "docs/css//*"    %> copyStatic
  "docs/js//*"     %> copyStatic
  where
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
