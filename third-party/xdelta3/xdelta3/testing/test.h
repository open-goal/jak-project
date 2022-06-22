/* xdelta3 - delta compression tools and library -*- Mode: C++ -*-
   Copyright 2016 Joshua MacDonald

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

extern "C" {
#include "../xdelta3.h"
#include "../xdelta3-internal.h"
}

#include <unistd.h>
#include <math.h>
#include <string>

#define CHECK_EQ(x,y) CHECK_OP(x,y,==)
#define CHECK_NE(x,y) CHECK_OP(x,y,!=)
#define CHECK_LT(x,y) CHECK_OP(x,y,<)
#define CHECK_GT(x,y) CHECK_OP(x,y,>)
#define CHECK_LE(x,y) CHECK_OP(x,y,<=)
#define CHECK_GE(x,y) CHECK_OP(x,y,>=)

#define CHECK_OP(x,y,OP) \
  do { \
    __typeof__(x) _x(x); \
    __typeof__(x) _y(y); \
    if (!(_x OP _y)) { \
      cerr << __FILE__ << ":" << __LINE__ << " Check failed: " << #x " " #OP " " #y << endl; \
      cerr << __FILE__ << ":" << __LINE__ << " {0} " << _x << endl; \
      cerr << __FILE__ << ":" << __LINE__ << " {1} " << _y << endl; \
    abort(); \
    } } while (false)
#undef CHECK
#define CHECK(x) \
  do {if (!(x)) {				       \
  cerr << __FILE__ << ":" << __LINE__ << " Check failed: " << #x << endl; \
  abort(); \
    } } while (false)

#define DCHECK(x)

using std::string;

#include <vector>
using std::vector;

inline string CommandToString(const vector<const char*> &v) {
  string s(v[0]);
  for (size_t i = 1; i < v.size() && v[i] != NULL; i++) {
    s.append(" ");
    s.append(v[i]);
  }
  return s;
}

#include <iostream>
using std::cerr;
using std::endl;
using std::ostream;

#include <map> 
using std::map;
using std::pair;

#include <list>
using std::list;

template <typename T, typename U>
pair<T, U> make_pair(const T& t, const U& u) {
  return pair<T, U>(t, u);
}

using std::min;
using std::max;
