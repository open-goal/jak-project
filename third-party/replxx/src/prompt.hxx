#ifndef REPLXX_PROMPT_HXX_INCLUDED
#define REPLXX_PROMPT_HXX_INCLUDED 1

#include <cstdlib>

#include "unicodestring.hxx"
#include "terminal.hxx"

namespace replxx {

class Prompt {           // a convenience struct for grouping prompt info
public:
	UnicodeString _text;   // our copy of the prompt text, edited
	int _characterCount;   // chars in _text
	int _byteCount;        // bytes in _text
	int _extraLines;       // extra lines (beyond 1) occupied by prompt
	int _indentation;      // column offset to end of prompt
	int _lastLinePosition; // index into _text where last line begins
	int _previousInputLen; // _characterCount of previous input line, for clearing
	int _cursorRowOffset;  // where the cursor is relative to the start of the prompt
	int _previousLen;      // help erasing
private:
	int _screenColumns;    // width of screen in columns [cache]
	Terminal& _terminal;
public:
	Prompt( Terminal& );
	void set_text( UnicodeString const& textPtr );
	void update_screen_columns( void );
	int screen_columns() const {
		return ( _screenColumns );
	}
	void write();
};

// changing prompt for "(reverse-i-search)`text':" etc.
//
struct DynamicPrompt : public Prompt {
	UnicodeString _searchText; // text we are searching for
	int _direction;            // current search _direction, 1=forward, -1=reverse

	DynamicPrompt( Terminal&, int initialDirection );
	void updateSearchPrompt(void);
};

}

#endif
