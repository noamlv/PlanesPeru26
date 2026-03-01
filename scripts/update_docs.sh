#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

quarto render site
rm -rf docs
mkdir -p docs
rsync -a --delete site/_site/ docs/
: > docs/.nojekyll
echo "docs actualizado desde site/_site"
