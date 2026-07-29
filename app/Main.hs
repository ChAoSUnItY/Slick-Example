{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import qualified Data.Map.Strict            as Map
import           Development.Shake
import           Development.Shake.Classes (Binary)
import           Development.Shake.Forward
import           Development.Shake.FilePath
import           GHC.Generics               (Generic)
import           Slick

import qualified Data.Text                  as T

outputFolder :: FilePath
outputFolder = "docs/"

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
    { posts    :: [Post]
    , tagCloud :: [TagCount]
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

-- | given a list of posts this will build a table of contents
buildIndex :: [Post] -> Action ()
buildIndex posts' = do
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo { posts = posts', tagCloud = buildTagCloud posts' }
      indexHTML = T.unpack $ substitute indexT (toJSON indexInfo)
  writeFile' (outputFolder </> "index.html") indexHTML

-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.md"]
  forP pPaths buildPost

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  postData <- markdownToHTML . T.pack $ postContent
  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl
  -- Add additional metadata we've been able to compute
  let fullPostData = withPostUrl $ postData
  template <- compileTemplate' "site/templates/post.html"
  writeFile' (outputFolder </> T.unpack postUrl) . T.unpack $ substitute template fullPostData
  -- Convert the metadata into a Post object
  convert fullPostData

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