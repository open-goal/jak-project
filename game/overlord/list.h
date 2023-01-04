#ifndef LIST_H_
#define LIST_H_

#include "common/common_types.h"

struct ListElement {
  ListElement *next, *prev;
};

struct List {
  char name[8];
  s32 sema;
  ListElement* l;
};

#endif  // LIST_H_
