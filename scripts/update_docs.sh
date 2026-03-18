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
cat > docs/robots.txt <<'EOF'
User-agent: *
Allow: /

Sitemap: https://noamlv.github.io/PlanesPeru26/sitemap.xml
EOF
cat > docs/sitemap.xml <<'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
  <url>
    <loc>https://noamlv.github.io/PlanesPeru26/</loc>
  </url>
  <url>
    <loc>https://noamlv.github.io/PlanesPeru26/partidos.html</loc>
  </url>
</urlset>
EOF
python3 - <<'PY'
from pathlib import Path

root = Path("docs")
base = "https://noamlv.github.io/PlanesPeru26"
image = f"{base}/assets/seo/og-planometro-2026.svg"
pages = {
    "index.html": {
        "url": f"{base}/",
        "title": "Planómetro 2026 | Análisis comparado de planes de gobierno en Perú",
        "description": "Visualizaciones y análisis comparado de los planes de gobierno en Perú 2026: propuestas, similitudes, factibilidad, métodos y planes originales."
    },
    "partidos.html": {
        "url": f"{base}/partidos.html",
        "title": "Planes originales | Planómetro 2026",
        "description": "Consulta los planes originales presentados ante el Jurado Nacional de Elecciones por cada candidatura presidencial."
    }
}

def upsert(html: str, meta_tag: str, key: str) -> str:
    if key in html:
        import re
        pattern = re.compile(rf'.*{re.escape(key)}.*\n?')
        html = pattern.sub('', html)
    return html.replace('</head>', meta_tag + '\n</head>')

for filename, meta in pages.items():
    path = root / filename
    html = path.read_text()
    for tag, key in [
        (f'<link rel="canonical" href="{meta["url"]}">', 'rel="canonical"'),
        ('<meta name="robots" content="index,follow,max-image-preview:large">', 'name="robots"'),
        (f'<meta property="og:url" content="{meta["url"]}">', 'property="og:url"'),
        ('<meta property="og:type" content="website">', 'property="og:type"'),
        (f'<meta property="og:title" content="{meta["title"]}">', 'property="og:title"'),
        (f'<meta property="og:description" content="{meta["description"]}">', 'property="og:description"'),
        (f'<meta property="og:image" content="{image}">', 'property="og:image"'),
        (f'<meta name="twitter:title" content="{meta["title"]}">', 'name="twitter:title"'),
        (f'<meta name="twitter:description" content="{meta["description"]}">', 'name="twitter:description"'),
        (f'<meta name="twitter:image" content="{image}">', 'name="twitter:image"'),
    ]:
        html = upsert(html, tag, key)
    path.write_text(html)
PY
echo "docs actualizado desde site/_site"
