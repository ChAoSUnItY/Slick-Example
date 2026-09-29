module Config
  ( buildOptions
  , categoryPagePath
  , homePagePath
  , indexTemplate
  , outputFolder
  , postOutput
  , postSourceForOutput
  , postTemplate
  , postsPerIndexPage
  , routeSegment
  , siteFolder
  , siteTitle
  , staticOutput
  , staticSourceForOutput
  , tagPagePath
  ) where

import           Data.Char                  (isAlphaNum, toLower)
import           Data.List                  (intercalate)
import           Development.Shake         (ShakeOptions, shakeOptions,
                                            shakeLintInside, shakeVersion)
import           Development.Shake.FilePath ((</>), (<.>), (-<.>), dropDirectory1,
                                             makeRelative)

siteFolder :: FilePath
siteFolder = "site"

outputFolder :: FilePath
outputFolder = "docs"

indexTemplate :: FilePath
indexTemplate = siteFolder </> "templates/index.html"

postTemplate :: FilePath
postTemplate = siteFolder </> "templates/post.html"

siteTitle :: String
siteTitle = "Field Notes"

-- | Maximum number of posts rendered on one index page.
postsPerIndexPage :: Int
postsPerIndexPage = 5

postOutput :: FilePath -> FilePath
postOutput source = outputFolder </> dropDirectory1 (source -<.> "html")

postSourceForOutput :: FilePath -> FilePath
postSourceForOutput out =
  (siteFolder </> "posts" </> makeRelative (outputFolder </> "posts") out)
    -<.> "md"

staticOutput :: FilePath -> FilePath
staticOutput source = outputFolder </> dropDirectory1 source

staticSourceForOutput :: FilePath -> FilePath
staticSourceForOutput out = siteFolder </> makeRelative outputFolder out

homePagePath :: Int -> FilePath
homePagePath page = show page <.> "html"

tagPagePath :: String -> Int -> FilePath
tagPagePath tag' page = "tag" </> tag' </> homePagePath page

categoryPagePath :: String -> Int -> FilePath
categoryPagePath category' page = "category" </> category' </> homePagePath page

buildOptions :: ShakeOptions
buildOptions = shakeOptions
  { shakeLintInside = ["."]
  , shakeVersion = "post-navigation-v1-" <> show postsPerIndexPage
  }

-- Helper functions

-- routeSegment converts input to url-friendly "slug" by lowering charcters 
-- and replacing whitespaces with "-"
routeSegment :: String -> String
routeSegment = intercalate "-" . words . map toSafeChar
  where
    toSafeChar c | isAlphaNum c = toLower c
                 | otherwise    = ' '
