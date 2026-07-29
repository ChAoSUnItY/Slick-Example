---
title: "Hello, Static World"
author: "Your Name"
date: Jul 29, 2026
tags: [slick, site]
description: My first blog post using slick
image: "/images/cover-lambda.svg"
---

This is the first post in your new **Slick** site. Everything you're reading right now is plain markdown, compiled at build time into a flat HTML file — no server, no database, no runtime to keep patched.

## What's already wired up

- A responsive, editorial layout with light and dark themes (the toggle lives in the top bar, top right)
- A reading-progress indicator on post pages
- Copy buttons on code blocks
- An index page that lists posts newest-first, driven entirely by the frontmatter below

## Frontmatter

Every post needs a YAML block at the top with `title`, `author`, `date`, and an optional `image`:

```yaml
---
title: "My Post Title"
author: "Your Name"
date: "29 Jul 2026"
image: "/images/cover-lambda.svg"
---
```

The `image` field is optional — omit it and the post simply renders without a cover.

## Code blocks work as expected

```haskell
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  postContent <- readFile' srcPath
  postData    <- markdownToHTML . T.pack $ postContent
  convert postData
```

Delete this file, add your own markdown into `site/posts/`, and run `stack exec build-site` to rebuild.
