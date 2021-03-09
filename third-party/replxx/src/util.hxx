#ifndef REPLXX_UTIL_HXX_INCLUDED
#define REPLXX_UTIL_HXX_INCLUDED 1

#include "replxx.hxx"

namespace replxx {

inline bool is_control_code(char32_t testChar) {
	return (testChar < ' ') ||											// C0 controls
				 (testChar >= 0x7F && testChar <= 0x9F);	// DEL and C1 controls
}

inline char32_t control_to_human( char32_t key ) {
	return ( key < 27 ? ( key + 0x40 ) : ( key + 0x18 ) );
}

void recompute_character_widths( char32_t const* text, char* widths, int charCount );
void calculate_screen_position( int x, int y, int screenColumns, int charCount, int& xOut, int& yOut );
int calculate_displayed_length( char32_t const* buf32, int size );
char const* ansi_color( Replxx::Color );
std::string now_ms_str( void );

}

#endif

