#ifndef REPLXX_HISTORY_HXX_INCLUDED
#define REPLXX_HISTORY_HXX_INCLUDED 1

#include <list>
#include <unordered_map>

#include "unicodestring.hxx"
#include "utf8string.hxx"
#include "conversion.hxx"
#include "util.hxx"

namespace std {
template<>
struct hash<replxx::UnicodeString> {
	std::size_t operator()( replxx::UnicodeString const& us_ ) const {
		std::size_t h( 0 );
		char32_t const* p( us_.get() );
		char32_t const* e( p + us_.length() );
		while ( p != e ) {
			h *= 31;
			h += *p;
			++ p;
		}
		return ( h );
	}
};
}

namespace replxx {

class History {
public:
	class Entry {
		std::string _timestamp;
		UnicodeString _text;
	public:
		Entry( std::string const& timestamp_, UnicodeString const& text_ )
			: _timestamp( timestamp_ )
			, _text( text_ ) {
		}
		std::string const& timestamp( void ) const {
			return ( _timestamp );
		}
		UnicodeString const& text( void ) const {
			return ( _text );
		}
		bool operator < ( Entry const& other_ ) const {
			return ( _timestamp < other_._timestamp );
		}
	};
	typedef std::list<Entry> entries_t;
	typedef std::unordered_map<UnicodeString, entries_t::const_iterator> locations_t;
private:
	entries_t _entries;
	locations_t _locations;
	int _maxSize;
	entries_t::const_iterator _current;
	entries_t::const_iterator _yankPos;
	/*
	 * _previous and _recallMostRecent are used to allow
	 * HISTORY_NEXT action (a down-arrow key) to have a special meaning
	 * if invoked after a line from history was accepted without
	 * any modification.
	 * Special meaning is: a down arrow shall jump to the line one
	 * after previously accepted from history.
	 */
	entries_t::const_iterator _previous;
	bool _recallMostRecent;
	bool _unique;
public:
	History( void );
	void add( UnicodeString const& line, std::string const& when = now_ms_str() );
	bool save( std::string const& filename, bool );
	bool load( std::string const& filename );
	void clear( void );
	void set_max_size( int len );
	void set_unique( bool unique_ ) {
		_unique = unique_;
		remove_duplicates();
	}
	void reset_yank_iterator();
	bool next_yank_position( void );
	void reset_recall_most_recent( void ) {
		_recallMostRecent = false;
	}
	void commit_index( void ) {
		_previous = _current;
		_recallMostRecent = true;
	}
	bool is_empty( void ) const {
		return ( _entries.empty() );
	}
	void update_last( UnicodeString const& );
	void drop_last( void );
	bool is_last( void ) const;
	bool move( bool );
	UnicodeString const& current( void ) const {
		return ( _current->text() );
	}
	UnicodeString const& yank_line( void ) const {
		return ( _yankPos->text() );
	}
	void jump( bool, bool = true );
	bool common_prefix_search( UnicodeString const&, int, bool );
	int size( void ) const {
		return ( static_cast<int>( _entries.size() ) );
	}
	Replxx::HistoryScan::impl_t scan( void ) const;
	void save_pos( void );
	void restore_pos( void );
private:
	History( History const& ) = delete;
	History& operator = ( History const& ) = delete;
	bool move( entries_t::const_iterator&, int, bool = false ) const;
	entries_t::const_iterator moved( entries_t::const_iterator, int, bool = false ) const;
	void erase( entries_t::const_iterator );
	void trim_to_max_size( void );
	void remove_duplicate( UnicodeString const& );
	void remove_duplicates( void );
	bool do_load( std::string const& );
	entries_t::const_iterator last( void ) const;
	void sort( void );
	void reset_iters( void );
};

class Replxx::HistoryScanImpl {
	History::entries_t const& _entries;
	History::entries_t::const_iterator _it;
	mutable Utf8String _utf8Cache;
	mutable Replxx::HistoryEntry _entryCache;
	mutable bool _cacheValid;
public:
	HistoryScanImpl( History::entries_t const& );
	bool next( void );
	Replxx::HistoryEntry const& get( void ) const;
};

}

#endif

