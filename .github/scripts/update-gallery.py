import glob
import os
from pathlib import Path
import json

galleryLinks = {
  'jak1': {
    'name': "Jak 1",
    'media': [],
  },
  'jak2': {
    'name': "Jak 2",
    'media': [],
  },
  'jak3': {
    'name': "Jak 3",
    'media': [],
  },
  'jakx': {
    'name': "Jak X",
    'media': [],
  },
  'misc': {
    'name': "Miscellaneous",
    'media': [],
  }
}

def get_links(key, folder_to_search):
  if os.path.isdir(folder_to_search):
    files = glob.glob(folder_to_search + "/*.png", recursive=True)
    files.extend(glob.glob(folder_to_search + "/*.jpg", recursive=True))
    files.extend(glob.glob(folder_to_search + "/*.jpeg", recursive=True))
    for f in files:
      galleryLinks[key]["media"].append({
        'fileName': os.path.basename(f),
        'timestamp': Path(f).stem.split("_")[1],
        'caption': Path(f).stem.split("_")[0].replace("-", " ").title(),
        'video': False
      })
    # get videos potentially
    if os.path.exists("{}/videos.json".format(folder_to_search)):
      with open("{}/videos.json".format(folder_to_search), 'r') as f:
        data = json.load(f)
        for video in data:
          galleryLinks[key]["media"].append({
            'link': video["link"].replace("watch?v=", "embed/"),
            'timestamp': video["timestamp"],
            'video': True
          })
    # sort by timestamp
    galleryLinks[key]["media"].sort(key=lambda x: x["timestamp"], reverse=True)

get_links('jak1', './docs/gh-pages-proj/src/assets/gallery/jak1')
get_links('jak2', './docs/gh-pages-proj/src/assets/gallery/jak2')
get_links('jak3', './docs/gh-pages-proj/src/assets/gallery/jak3')
get_links('jakx', './docs/gh-pages-proj/src/assets/gallery/jakx')
get_links('misc', './docs/gh-pages-proj/src/assets/gallery/misc')

with open('./docs/gh-pages-proj/src/config/gallery.json', 'r+', encoding='utf-8') as f:
  f.seek(0)
  json.dump(galleryLinks, f, ensure_ascii=False, indent=2)
  f.truncate()
