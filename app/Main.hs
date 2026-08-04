{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Control.Applicative        ((<|>))
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Data.List                  (sortBy)
import qualified Data.Map.Strict            as Map
import           Data.Time                  (Day, defaultTimeLocale, parseTimeM)
import           Development.Shake
import           Development.Shake.Classes (Binary)
import           Development.Shake.Forward
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
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | Data for the index page
data IndexInfo =
  IndexInfo
    { posts             :: [Post]
    , tagCloud          :: [TagCount]
    , indexPostsPerPage :: Int
    } deriving (Generic, Show, FromJSON, ToJSON)

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

-- | Build a tag cloud from every post's tags: alphabetical and deduped,
-- with a 1-5 "weight" bucket sized relative to the most-used tag.
buildTagCloud :: [Post] -> [TagCount]
buildTagCloud ps =
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
  in [ TagCount t n (bucket n) | (t, n) <- counts ]

-- | Build the index page with the newest posts first. Client-side JavaScript
-- uses `indexPostsPerPage` to paginate the rendered entries without loading a
-- separate index document.
buildIndex :: [Post] -> Action ()
buildIndex posts' = do
  indexT <- compileTemplate' "site/templates/index.html"
  let sortedPosts = sortPostsByDate posts'
      indexInfo = IndexInfo
        { posts = sortedPosts
        , tagCloud = buildTagCloud sortedPosts
        , indexPostsPerPage = postsPerIndexPage
        }
      indexHTML = T.unpack $ substitute indexT (toJSON indexInfo)
  writeFile' (outputFolder </> "index.html") indexHTML

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

-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.md"]
  posts' <- forP pPaths loadPost
  let sortedPosts = sortPostsByDate posts'
      adjacentPosts = zip3
        (Nothing : map Just sortedPosts)
        sortedPosts
        (map Just (drop 1 sortedPosts) ++ [Nothing])
  void $ forP adjacentPosts $ \(newerPost, post, olderPost) ->
    writePost post olderPost newerPost
  pure sortedPosts

-- | Load and process a post's Markdown and frontmatter.
loadPost :: FilePath -> Action Post
loadPost srcPath = cacheAction ("load" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Loading post: " <> srcPath
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

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
    filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*"]
    void $ forP filepaths $ \filepath ->
        copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  allPosts <- buildPosts
  buildIndex allPosts
  copyStaticFiles

main :: IO ()
main = shakeArgsForward
    shakeOptions { shakeLintInside = ["."] }
    buildRules
