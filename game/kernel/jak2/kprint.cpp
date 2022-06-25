#include "kprint.h"

#include <cstdio>

#include "common/listener_common.h"

#include "game/kernel/common/fileio.h"
#include "game/kernel/common/kboot.h"
#include "game/kernel/common/kprint.h"

void output_sql_query(char* query_name) {
  if (MasterDebug != 0) {
    sprintf(strend(OutputBufArea.cast<char>().c() + sizeof(ListenerMessageHeader)), "sql-query \"");

    char* buffer_ptr = strend(OutputBufArea.cast<char>().c() + sizeof(ListenerMessageHeader));
    char query_char = *query_name;
    char query_char_to_add = *query_name;
    while (query_char != 0) {
      if (query_char_to_add == '\"') {
        *buffer_ptr = '\\';
        buffer_ptr[1] = *query_name;
        buffer_ptr += 2;
      } else {
        *buffer_ptr = query_char_to_add;
        buffer_ptr++;
      }
      query_name = query_name + 1;
      query_char = *query_name;
      query_char_to_add = *query_name;
    }
    *buffer_ptr = '\"';
    buffer_ptr[1] = '\n';
    buffer_ptr[2] = 0;
    OutputPending = OutputBufArea + sizeof(ListenerMessageHeader);
  }
}
