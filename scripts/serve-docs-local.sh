#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DOCS_DIR="$ROOT/docs"
SITE_DIR="$DOCS_DIR/_site"
HOST="${HOST:-127.0.0.1}"
PORT="${PORT:-8099}"
BUILD_ONLY=0
FORCE_FALLBACK=0

usage() {
  cat <<'EOF'
Usage: scripts/serve-docs-local.sh [options]

Build and serve the Magent documentation site locally.

Options:
  --host HOST       Bind host. Default: 127.0.0.1
  --port PORT       Preferred port. Default: 8099
  --build-only      Build docs/_site and exit without serving
  --fallback        Force the lightweight Kramdown preview builder
  -h, --help        Show this help

Environment:
  HOST=0.0.0.0 PORT=4000 scripts/serve-docs-local.sh

The script prefers Jekyll when available, matching GitHub Pages more closely.
When Jekyll is unavailable, it falls back to a small Kramdown-based builder and
serves docs/_site with Python's built-in HTTP server.
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --host)
      HOST="${2:?missing value for --host}"
      shift 2
      ;;
    --port)
      PORT="${2:?missing value for --port}"
      shift 2
      ;;
    --build-only)
      BUILD_ONLY=1
      shift
      ;;
    --fallback)
      FORCE_FALLBACK=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

require_cmd() {
  if ! command -v "$1" >/dev/null 2>&1; then
    echo "Missing required command: $1" >&2
    exit 1
  fi
}

choose_port() {
  require_cmd python3
  python3 - "$HOST" "$PORT" <<'PY'
import socket
import sys

host = sys.argv[1]
start = int(sys.argv[2])

for port in range(start, start + 100):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    try:
        sock.bind((host, port))
    except OSError:
        sock.close()
        continue
    sock.close()
    print(port)
    sys.exit(0)

raise SystemExit(f"no free port found from {start} to {start + 99}")
PY
}

build_with_jekyll() {
  local jekyll_cmd
  jekyll_cmd="$(command -v jekyll || true)"
  if [[ -z "$jekyll_cmd" ]]; then
    return 1
  fi

  if [[ "$BUILD_ONLY" -eq 1 ]]; then
    "$jekyll_cmd" build --source "$DOCS_DIR" --destination "$SITE_DIR"
  else
    local actual_port
    actual_port="$(choose_port)"
    echo "Serving Magent docs with Jekyll at http://$HOST:$actual_port/"
    exec "$jekyll_cmd" serve \
      --source "$DOCS_DIR" \
      --destination "$SITE_DIR" \
      --host "$HOST" \
      --port "$actual_port"
  fi
}

build_with_kramdown() {
  if ! ruby -e 'require "yaml"; require "erb"; require "kramdown"; require "kramdown-parser-gfm"' >/dev/null 2>&1; then
    cat >&2 <<'EOF'
Jekyll is not installed, and the fallback builder dependencies are missing.

Install either:
  gem install --user-install jekyll

or the fallback dependencies:
  gem install --user-install kramdown kramdown-parser-gfm
EOF
    exit 1
  fi

  ruby - "$DOCS_DIR" "$SITE_DIR" <<'RUBY'
require 'fileutils'
require 'yaml'
require 'erb'
require 'kramdown'
require 'kramdown-parser-gfm'

docs_dir = File.expand_path(ARGV.fetch(0))
site_dir = File.expand_path(ARGV.fetch(1))
nav = YAML.load_file(File.join(docs_dir, '_data/navigation.yml'))

FileUtils.rm_rf(site_dir) if File.basename(site_dir) == '_site'
FileUtils.mkdir_p(File.join(site_dir, 'assets/css'))
FileUtils.cp(File.join(docs_dir, 'assets/css/site.css'),
             File.join(site_dir, 'assets/css/site.css'))

def parse_page(path)
  text = File.read(path)
  if text.start_with?("---\n")
    _empty, front_matter, body = text.split(/^---\s*\n/, 3)
    [YAML.safe_load(front_matter, permitted_classes: [Date, Time],
                                  aliases: true) || {}, body]
  else
    [{}, text]
  end
end

def output_path(site_dir, file, meta)
  case meta['permalink']
  when '/'
    File.join(site_dir, 'index.html')
  when '/zh/'
    File.join(site_dir, 'zh', 'index.html')
  else
    File.join(site_dir, File.basename(file, '.md') + '.html')
  end
end

def page_url(file, meta)
  meta['permalink'] || '/' + File.basename(file, '.md') + '.html'
end

def escape_html(value)
  ERB::Util.html_escape(value.to_s)
end

Dir.glob(File.join(docs_dir, '*.md')).sort.each do |file|
  meta, body = parse_page(file)
  html_body = Kramdown::Document.new(body, input: 'GFM',
                                           syntax_highlighter: 'rouge').to_html
  lang = meta['lang'] || 'en'
  nav_items = nav.fetch(lang, nav.fetch('en'))
  url = page_url(file, meta)
  title = meta['title'] || File.basename(file, '.md')
  alt_url = meta['alt_url'] || (lang == 'zh' ? '/' : '/zh/')
  language_label = lang == 'zh' ? 'English' : '中文'
  brand_href = lang == 'zh' ? '/zh/' : '/'
  nav_html = nav_items.map do |item|
    current = item['url'] == url ? ' aria-current="page"' : ''
    %(<a href="#{escape_html(item['url'])}"#{current}>#{escape_html(item['title'])}</a>)
  end.join("\n")

  document = <<~HTML
    <!doctype html>
    <html lang="#{escape_html(lang)}">
      <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <title>#{escape_html(title)} | Magent Documentation</title>
        <meta name="description" content="Emacs-native AI coding agent documentation">
        <link rel="stylesheet" href="/assets/css/site.css">
      </head>
      <body>
        <a class="skip-link" href="#content">Skip to content</a>
        <header class="topbar">
          <a class="brand" href="#{escape_html(brand_href)}">
            <span class="brand-mark">M</span>
            <span>
              <strong>Magent</strong>
              <small>eMacs-native LLM AGENT</small>
            </span>
          </a>
          <nav class="top-actions" aria-label="Language">
            <a class="language-switch" href="#{escape_html(alt_url)}">#{escape_html(language_label)}</a>
          </nav>
        </header>
        <div class="page-shell">
          <aside class="sidebar" aria-label="Documentation">
            <nav>#{nav_html}</nav>
          </aside>
          <main id="content" class="content">
            <article class="doc-card">#{html_body}</article>
          </main>
        </div>
      </body>
    </html>
  HTML

  destination = output_path(site_dir, file, meta)
  FileUtils.mkdir_p(File.dirname(destination))
  File.write(destination, document)
end

puts "Built preview site in #{site_dir}"
RUBY
}

serve_static_site() {
  require_cmd python3
  local actual_port
  actual_port="$(choose_port)"
  echo "Serving Magent docs at http://$HOST:$actual_port/"
  cd "$SITE_DIR"
  exec python3 -m http.server "$actual_port" --bind "$HOST"
}

if [[ "$FORCE_FALLBACK" -eq 0 ]]; then
  if build_with_jekyll; then
    exit 0
  fi
fi

build_with_kramdown
if [[ "$BUILD_ONLY" -eq 0 ]]; then
  serve_static_site
fi
