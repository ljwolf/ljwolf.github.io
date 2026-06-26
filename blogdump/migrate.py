#!/usr/bin/env python3
"""One-off migration of parsed Tumblr posts -> Hugo content/posts/.

Reads blogdump/postbypost/*.md (front matter + raw HTML body), cleans the
front matter, rewrites Tumblr CDN image references to locally-copied files
under static/images/archive/, replaces the malformed "imported from" footer
with a clean attribution line, and writes the result to content/posts/.

Stdlib only. Run from repo root:  python blogdump/migrate.py
"""
import json
import os
import re
import shutil
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC = os.path.join(ROOT, "blogdump", "postbypost")
DUMP = os.path.join(ROOT, "blogdump")
OUT = os.path.join(ROOT, "content", "posts")
ARCHIVE = os.path.join(ROOT, "static", "images", "archive")

IMG_EXTS = (".gif", ".png", ".jpg", ".jpeg")
SIZE_SUFFIX = re.compile(r"_(raw|\d+)$")

# Meta tags that are migration artifacts, not topical tags.
DROP_TAGS = {"imported", "image"}


def base_id(filename):
    """tumblr_ncql4nMXCj1ts05oao2_1280.png -> tumblr_ncql4nMXCj1ts05oao2"""
    stem = os.path.splitext(filename)[0]
    return SIZE_SUFFIX.sub("", stem)


def build_local_index():
    """Map base tumblr id -> on-disk filename for downloaded full-res images."""
    index = {}
    for name in os.listdir(DUMP):
        if name.lower().endswith(IMG_EXTS):
            index.setdefault(base_id(name), name)
    return index


def parse_front_matter(text):
    """Split a `---`-delimited front matter block from the body."""
    m = re.match(r"^---\n(.*?)\n---\n(.*)$", text, re.DOTALL)
    if not m:
        raise ValueError("no front matter")
    fm_raw, body = m.group(1), m.group(2)
    fm = {}
    for line in fm_raw.splitlines():
        key, _, val = line.partition(":")
        fm[key.strip()] = val.strip()
    return fm, body


def humanize(slug):
    return " ".join(w.capitalize() for w in slug.split("-"))


def slugify(s):
    s = re.sub(r"[^a-z0-9]+", "-", s.lower())
    return s.strip("-")


def clean_tags(raw):
    if not raw:
        return []
    try:
        tags = json.loads(raw)
    except json.JSONDecodeError:
        return []
    out = []
    seen = set()
    for t in tags:
        t = t.strip()
        if not t or t.lower() in DROP_TAGS or t.lower() in seen:
            continue
        seen.add(t.lower())
        out.append(t)
    return out


# Matches the (malformed) export footer through end of string.
FOOTER_RE = re.compile(r"\s*<small><i>\s*imported from:.*$", re.DOTALL)
# <img src=//host/hash/tumblr_xxx_1280.png  (quoted or bare)
IMG_SRC_RE = re.compile(r'src=(?:"([^"]+)"|(\S+))')


def rewrite_images(body, local_index, copied, unmatched, slug):
    def repl(m):
        url = m.group(1) or m.group(2)
        # A bare (unquoted) src like  src=//...png>  glues the tag-closing
        # `>` onto the URL. Strip it for matching, but re-emit it so the
        # <img> tag stays well-formed.
        tail = ""
        if m.group(2) and url.endswith(">"):
            url = url[:-1]
            tail = ">"
        fname = url.split("/")[-1]
        if not fname.lower().endswith(IMG_EXTS):
            return m.group(0)
        bid = base_id(fname)
        local = local_index.get(bid)
        if local:
            copied.add(local)
            return 'src="/images/archive/%s"%s' % (local, tail)
        # No local full-res copy; keep external but note it.
        unmatched.append((slug, url))
        https = url
        if https.startswith("//"):
            https = "https:" + https
        elif https.startswith("http://"):
            https = "https://" + https[len("http://"):]
        return 'src="%s"%s' % (https, tail)

    return IMG_SRC_RE.sub(repl, body)


def main():
    os.makedirs(ARCHIVE, exist_ok=True)
    local_index = build_local_index()
    copied = set()
    unmatched = []
    n = 0
    empty_titles = 0

    for fn in sorted(os.listdir(SRC)):
        if not fn.endswith(".md"):
            continue
        with open(os.path.join(SRC, fn), encoding="utf-8") as f:
            fm, body = parse_front_matter(f.read())

        date = fm.get("date", "")
        pid = fm.get("id", "")

        url = fm.get("url", "").strip()
        # Some exports lack a usable url; fall back to a stable id-based slug.
        slug = slugify(url) or ("post-%s" % pid)
        title = fm.get("title", "").strip()
        if not title:
            title = humanize(slug) if url else "Untitled photo post"
            empty_titles += 1
        tags = clean_tags(fm.get("tags", ""))

        body = FOOTER_RE.sub("", body).rstrip()
        body = rewrite_images(body, local_index, copied, unmatched, slug)

        attribution = (
            "\n\n*Originally posted on "
            "[yetanothergeographer.tumblr.com]"
            "(https://yetanothergeographer.tumblr.com/%s/%s).*\n" % (pid, slug)
        )

        # Build clean YAML front matter.
        title_yaml = title.replace('"', '\\"')
        tags_yaml = "[" + ", ".join('"%s"' % t for t in tags) + "]"
        fm_out = (
            "---\n"
            'title: "%s"\n'
            "date: %s\n"
            "tags: %s\n"
            "draft: false\n"
            "---\n" % (title_yaml, date, tags_yaml)
        )

        out_path = os.path.join(OUT, slug + ".md")
        with open(out_path, "w", encoding="utf-8") as f:
            f.write(fm_out + "\n" + body + attribution)
        n += 1

    # Copy matched images.
    for name in sorted(copied):
        shutil.copy2(os.path.join(DUMP, name), os.path.join(ARCHIVE, name))

    print("Migrated %d posts (%d had empty titles -> humanized slug)." % (n, empty_titles))
    print("Copied %d images into static/images/archive/." % len(copied))
    if unmatched:
        print("\n%d image refs had NO local full-res match (kept external):" % len(unmatched))
        for slug, url in unmatched:
            print("  [%s] %s" % (slug, url))
    else:
        print("All Tumblr image references matched a local file.")


if __name__ == "__main__":
    main()
