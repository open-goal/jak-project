#ifndef JAK_V2_STATICRECORD_H
#define JAK_V2_STATICRECORD_H

struct StaticLinkRecord {
  enum Kind { TYPE_PTR, SYMBOL_PTR } kind;

  StaticLinkRecord() = default;
  StaticLinkRecord(Kind _kind, int _offset) : kind(_kind), offset(_offset) {}

  int offset = -1;
};

#endif  // JAK_V2_STATICRECORD_H
