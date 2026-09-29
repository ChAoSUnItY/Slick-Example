# Slick Example

A static blog built with [Slick](https://github.com/ChrisPenner/slick) and Shake. Edit Markdown posts and templates under `site/`; the generated site lives in `docs/`.

## Build and preview

```sh
stack build
stack exec build-site
```

Serve `docs/` with any static file server to preview the result. The repository can publish that directory with GitHub Pages.

## Posts and configuration

Add a Markdown file to `site/posts/` with front matter containing `title`, `author`, `date`, and one `category`. A post may also have `tags` and an `image`. Existing posts provide examples.

Set the site title, output paths, and `postsPerIndexPage` in [`app/Config.hs`](app/Config.hs). The listing and post templates are in `site/templates/`; styles, scripts, and images are under `site/`.

## Listings and pagination

Shake generates a home listing at `/1.html`, tag listings at `/tag/<slug>/1.html`, and category listings at `/category/<slug>/1.html`. Page numbers start at 1. Each listing has `ceil(post count / postsPerIndexPage)` numbered pages; the empty home listing still has page 1. Every listing's `index.html` is an exact copy of its `1.html`.

The build writes listing manifests under `_build/listings/` and per-post navigation manifests under `_build/navigation/`. Shake uses these to calculate pages and re-render only post pages whose content or neighbour links changed. It removes obsolete generated pages, navigation manifests, and assets when the site changes. `_build/` and `.shake/` are local build state and are ignored by Git. If you change the Haskell generator itself, rebuild the executable with `stack build`; changing its rules or rendering logic may also require a `shakeVersion` bump in `app/Config.hs` to invalidate cached outputs.
