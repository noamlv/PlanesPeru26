#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

quarto render site
mkdir -p site/_site/assets
rsync -a site/assets/ site/_site/assets/
mkdir -p site/_site/assets/plans_pdf
find site/_site/assets/plans_pdf -mindepth 1 -maxdepth 1 -type f -delete
python3 - <<'PY'
import re
import shutil
import unicodedata
from pathlib import Path

src_dir = Path("Planes")
dst_dir = Path("site/_site/assets/plans_pdf")
dst_dir.mkdir(parents=True, exist_ok=True)

def slugify(text: str) -> str:
    text = unicodedata.normalize("NFKD", text).encode("ascii", "ignore").decode("ascii")
    text = text.lower()
    text = re.sub(r"[^a-z0-9]+", "-", text).strip("-")
    return text

for pdf in sorted(src_dir.glob("*.pdf")):
    target = dst_dir / f"{slugify(pdf.stem)}.pdf"
    shutil.copy2(pdf, target)
PY
rm -rf docs
mkdir -p docs
rsync -a --delete site/_site/ docs/
: > docs/.nojekyll
echo "docs actualizado desde site/_site"
