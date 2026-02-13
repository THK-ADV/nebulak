# nebulak

A small parsing library highly inspired by https://www.pointfree.co

## Publishing to GitHub Packages

To publish new versions (`sbt publish`), provide GitHub credentials in one of these ways:

1. **`.env` file** (recommended): Copy `.env.example` to `.env` and set `GITHUB_PACKAGES_TOKEN` (and optionally `GITHUB_PACKAGES_USER`, default `THK-ADV`). Use a [Personal Access Token](https://github.com/settings/tokens) with `write:packages` scope. `.env` is gitignored.

2. **Environment variables**: Set `GITHUB_PACKAGES_TOKEN` (or `GITHUB_TOKEN`) and optionally `GITHUB_PACKAGES_USER` before running `sbt publish`.