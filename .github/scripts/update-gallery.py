import glob
import os
from pathlib import Path
import datetime

galleryLinks = {
  'jak1': [],
  'jak2': [],
  'jak3': [],
  'jakx': [],
  'misc': []
}

def get_links(key, folder_to_search):
  if os.path.isdir(folder_to_search):
    files = [f for f in glob.glob(folder_to_search + "/*.*", recursive=True)]
    for f in files:
      galleryLinks[key].append({
        'fileName': os.path.basename(f),
        'lastModified': datetime.datetime.fromtimestamp(os.path.getmtime(f)).isoformat(),
        'caption': Path(f).stem.replace("-", " ").title()
      })

get_links('jak1', './docs/gh-pages-proj/src/assets/gallery/jak1')
get_links('jak2', './docs/gh-pages-proj/src/assets/gallery/jak2')
get_links('jak3', './docs/gh-pages-proj/src/assets/gallery/jak3')
get_links('jakx', './docs/gh-pages-proj/src/assets/gallery/jakx')
get_links('misc', './docs/gh-pages-proj/src/assets/gallery/misc')

import json
with open('./docs/gh-pages-proj/src/config/gallery.json', 'r+', encoding='utf-8') as f:
  f.seek(0)
  json.dump(galleryLinks, f, ensure_ascii=False, indent=2)
  f.truncate()
