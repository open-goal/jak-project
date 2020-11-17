#pragma once

class ObjectFileData;

struct TPageResultStats {
  int total_textures = 0;
  int successful_textures = 0;
};

TPageResultStats process_tpage(ObjectFileData& data);