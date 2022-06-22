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

class Mutator {
public:
  virtual ~Mutator() { }
  virtual void Mutate(SegmentMap *table,
		      const SegmentMap *source_table,
		      MTRandom *rand) const = 0;
};

class Change {
public:
  enum Kind {
    MODIFY = 1,     // Mutate a certain range w/ random or supplied data
    ADD = 2,        // Insert random or supplied data
    DELRANGE = 3,     // Delete a specified range of data
    COPY = 4,       // Copy from one region, inserting elsewhere
    MOVE = 5,       // Copy then delete copied-from range
    COPYOVER = 6    // Copy then delete copied-to range

    // ADD, DELRANGE, and COPY change the file size
    // MODIFY, MOVE, COPYOVER preserve the file size
  };

  // Constructor for modify, add, delete.
  Change(Kind kind0, xoff_t size0, xoff_t addr1_0)
    : kind(kind0),
      size(size0),
      addr1(addr1_0),
      addr2(0),
      insert(NULL) {
    CHECK(kind != MOVE && kind != COPY && kind != COPYOVER);
  }

  // Constructor for modify, add w/ provided data.
  Change(Kind kind0, xoff_t size0, xoff_t addr1_0, Segment *insert0)
    : kind(kind0),
      size(size0),
      addr1(addr1_0),
      addr2(0),
      insert(insert0) {
    CHECK(kind != MOVE && kind != COPY && kind != COPYOVER);
  }

  // Constructor for move, copy, overwrite
  Change(Kind kind0, xoff_t size0, xoff_t addr1_0, xoff_t addr2_0)
    : kind(kind0),
      size(size0),
      addr1(addr1_0),
      addr2(addr2_0),
      insert(NULL) {
    CHECK(kind == MOVE || kind == COPY || kind == COPYOVER);
  }

  Kind kind;
  xoff_t size;
  xoff_t addr1;
  xoff_t addr2;
  Segment *insert;  // For modify and/or add
};

typedef list<Change> ChangeList;
typedef typename ChangeList::const_iterator ConstChangeListIterator;
typedef typename ChangeList::iterator ChangeListIterator;

class ChangeListMutator : public Mutator {
public:
  ChangeListMutator(const ChangeList &cl)
    : cl_(cl) { }

  ChangeListMutator() { }

  void Mutate(SegmentMap *table,
	      const SegmentMap *source_table,
	      MTRandom *rand) const {
    // The speed of processing gigabytes of data is so slow compared with
    // these table-copy operations, no attempt to make this fast.
    SegmentMap tmp;

    for (ConstChangeListIterator iter(cl_.begin());
	 iter != cl_.end(); ++iter) {
      const Change &ch = *iter;
      tmp.clear();
      Mutate(ch, &tmp, source_table, rand);
      tmp.swap(*table);
      source_table = table;
    }
  }

  static void Mutate(const Change &ch,
		     SegmentMap *table,
		     const SegmentMap *source_table,
		     MTRandom *rand) {
    switch (ch.kind) {
    case Change::ADD:
      AddChange(ch, table, source_table, rand);
      break;
    case Change::MODIFY:
      ModifyChange(ch, table, source_table, rand);
      break;
    case Change::DELRANGE:
      DeleteChange(ch, table, source_table, rand);
      break;
    case Change::COPY:
      CopyChange(ch, table, source_table, rand);
      break;
    case Change::MOVE:
      MoveChange(ch, table, source_table, rand);
      break;
    case Change::COPYOVER:
      OverwriteChange(ch, table, source_table, rand);
      break;
    }
  }

  static void ModifyChange(const Change &ch,
			   SegmentMap *table,
			   const SegmentMap *source_table,
			   MTRandom *rand) {
    xoff_t m_start = ch.addr1;
    xoff_t m_end = m_start + ch.size;
    xoff_t i_start = 0;
    xoff_t i_end = 0;

    for (ConstSegmentMapIterator iter(source_table->begin());
	 iter != source_table->end();
	 ++iter) {
      const Segment &seg = iter->second;
      i_start = iter->first;
      i_end = i_start + seg.Size();

      if (i_end <= m_start || i_start >= m_end) {
	table->insert(table->end(), make_pair(i_start, seg));
	continue;
      }

      if (i_start < m_start) {
	table->insert(table->end(),
		      make_pair(i_start,
				seg.Subseg(0, m_start - i_start)));
      }

      // Insert the entire segment, even though it may extend into later
      // segments.  This condition avoids inserting it during later
      // segments.
      if (m_start >= i_start) {
	if (ch.insert != NULL) {
	  table->insert(table->end(), make_pair(m_start, *ch.insert));
	} else {
	  Segment part(m_end - m_start, rand);
	  table->insert(table->end(), make_pair(m_start, part));
	}
      }

      if (i_end > m_end) {
	table->insert(table->end(),
		      make_pair(m_end,
				seg.Subseg(m_end - i_start, i_end - m_end)));
      }
    }

    // This check verifies that the modify does not extend past the
    // source_table EOF.
    CHECK_LE(m_end, i_end);
  }

  static void AddChange(const Change &ch,
			SegmentMap *table,
			const SegmentMap *source_table,
			MTRandom *rand) {
    xoff_t m_start = ch.addr1;
    xoff_t i_start = 0;
    xoff_t i_end = 0;

    for (ConstSegmentMapIterator iter(source_table->begin());
	 iter != source_table->end();
	 ++iter) {
      const Segment &seg = iter->second;
      i_start = iter->first;
      i_end = i_start + seg.Size();

      if (i_end <= m_start) {
	table->insert(table->end(), make_pair(i_start, seg));
	continue;
      }

      if (i_start > m_start) {
	table->insert(table->end(), make_pair(i_start + ch.size, seg));
	continue;
      }

      if (i_start < m_start) {
	table->insert(table->end(),
		      make_pair(i_start,
				seg.Subseg(0, m_start - i_start)));
      }

      if (ch.insert != NULL) {
	table->insert(table->end(), make_pair(m_start, *ch.insert));
      } else {
	Segment addseg(ch.size, rand);
	table->insert(table->end(), make_pair(m_start, addseg));
      }

      if (m_start < i_end) {
	table->insert(table->end(),
		      make_pair(m_start + ch.size,
				seg.Subseg(m_start - i_start,
					   i_end - m_start)));
      }
    }

    CHECK_LE(m_start, i_end);

    // Special case for add at end-of-input.
    if (m_start == i_end) {
      Segment addseg(ch.size, rand);
      table->insert(table->end(), make_pair(m_start, addseg));
    }
  }

  static void DeleteChange(const Change &ch,
			   SegmentMap *table,
			   const SegmentMap *source_table,
			   MTRandom *rand) {
    xoff_t m_start = ch.addr1;
    xoff_t m_end = m_start + ch.size;
    xoff_t i_start = 0;
    xoff_t i_end = 0;

    for (ConstSegmentMapIterator iter(source_table->begin());
	 iter != source_table->end();
	 ++iter) {
      const Segment &seg = iter->second;
      i_start = iter->first;
      i_end = i_start + seg.Size();

      if (i_end <= m_start) {
	table->insert(table->end(), make_pair(i_start, seg));
	continue;
      }

      if (i_start >= m_end) {
	table->insert(table->end(), make_pair(i_start - ch.size, seg));
	continue;
      }

      if (i_start < m_start) {
	table->insert(table->end(),
		      make_pair(i_start,
				seg.Subseg(0, m_start - i_start)));
      }

      if (i_end > m_end) {
	table->insert(table->end(),
		      make_pair(m_end - ch.size,
				seg.Subseg(m_end - i_start, i_end - m_end)));
      }
    }

    CHECK_LT(m_start, i_end);
    CHECK_LE(m_end, i_end);
  }

  // A move is a copy followed by delete of the copied-from range.
  static void MoveChange(const Change &ch,
			 SegmentMap *table,
			 const SegmentMap *source_table,
			 MTRandom *rand) {
    SegmentMap tmp;
    CHECK_NE(ch.addr1, ch.addr2);
    CopyChange(ch, &tmp, source_table, rand);
    Change d(Change::DELRANGE, ch.size,
	     ch.addr1 < ch.addr2 ? ch.addr1 : ch.addr1 + ch.size);
    DeleteChange(d, table, &tmp, rand);
  }

  // An overwrite is a copy followed by a delete of the copied-to range.
  static void OverwriteChange(const Change &ch,
			      SegmentMap *table,
			      const SegmentMap *source_table,
			      MTRandom *rand) {
    SegmentMap tmp;
    CHECK_NE(ch.addr1, ch.addr2);
    CopyChange(ch, &tmp, source_table, rand);
    Change d(Change::DELRANGE, ch.size, ch.addr2 + ch.size);
    DeleteChange(d, table, &tmp, rand);
  }

  static void CopyChange(const Change &ch,
			 SegmentMap *table,
			 const SegmentMap *source_table,
			 MTRandom *ignore) {
    xoff_t m_start = ch.addr2;
    xoff_t c_start = ch.addr1;
    xoff_t i_start = 0;
    xoff_t i_end = 0;

    // Like AddChange() with AppendCopy instead of a random segment.
    for (ConstSegmentMapIterator iter(source_table->begin());
	 iter != source_table->end();
	 ++iter) {
      const Segment &seg = iter->second;
      i_start = iter->first;
      i_end = i_start + seg.Size();

      if (i_end <= m_start) {
	table->insert(table->end(), make_pair(i_start, seg));
	continue;
      }

      if (i_start > m_start) {
	table->insert(table->end(), make_pair(i_start + ch.size, seg));
	continue;
      }

      if (i_start < m_start) {
	table->insert(table->end(),
		      make_pair(i_start,
				seg.Subseg(0, m_start - i_start)));
      }

      AppendCopy(table, source_table, c_start, m_start, ch.size);

      if (m_start < i_end) {
	table->insert(table->end(),
		      make_pair(m_start + ch.size,
				seg.Subseg(m_start - i_start, i_end - m_start)));
      }
    }

    CHECK_LE(m_start, i_end);

    // Special case for copy to end-of-input.
    if (m_start == i_end) {
      AppendCopy(table, source_table, c_start, m_start, ch.size);
    }
  }

  static void AppendCopy(SegmentMap *table,
			 const SegmentMap *source_table,
			 xoff_t copy_offset,
			 xoff_t append_offset,
			 xoff_t length) {
    ConstSegmentMapIterator pos(source_table->upper_bound(copy_offset));
    --pos;
    xoff_t got = 0;

    while (got < length) {
      size_t seg_offset = copy_offset - pos->first;
      size_t advance = min(pos->second.Size() - seg_offset,
			   (size_t)(length - got));

      table->insert(table->end(),
		    make_pair(append_offset,
			      pos->second.Subseg(seg_offset,
						 advance)));

      got += advance;
      copy_offset += advance;
      append_offset += advance;
      ++pos;
    }
  }

  ChangeList* Changes() {
    return &cl_;
  }

  const ChangeList* Changes() const {
    return &cl_;
  }

private:
  ChangeList cl_;
};

class Modify1stByte : public Mutator {
public:
  void Mutate(SegmentMap *table,
	      const SegmentMap *source_table,
	      MTRandom *rand) const {
    ChangeListMutator::Mutate(Change(Change::MODIFY, 1, 0),
			      table, source_table, rand);
  }
};
