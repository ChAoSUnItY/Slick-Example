---
title: "Why I Went Back to Static Files"
author: "Your Name"
date: Jul 12, 2026
tags: [slick, site]
description: My first blog post using slick
image: "/images/cover-graph.svg"
---

A site built with Shake has one property that's easy to undersell: **every build is a proof**. If `stack exec build-site` finishes, the output directory is a complete, correct rendering of everything in `site/` — no half-migrated database, no cache invalidation bugs, nothing running that you didn't just watch compile.

## The trade-off, honestly

You give up:

1. Comments, without bolting on a third-party widget
2. Search, unless you build or embed one
3. Anything that needs to change without a rebuild

You get back a site that's nearly impossible to break in production, because there's no production process to break — just files on a CDN.

> The best kind of downtime is the kind that can't happen because there's nothing running.

## This template

This starter wires the Slick/Shake pipeline you already had to a real front end: `site/templates/index.html` and `site/templates/post.html` are Mustache templates, `site/css/style.css` and `site/js/main.js` get copied straight through to `docs/`, and posts live as markdown in `site/posts/`. Nothing here needs Node, a bundler, or a package.json.
