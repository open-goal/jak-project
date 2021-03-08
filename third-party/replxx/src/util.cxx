#include <chrono>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <wctype.h>

#include "util.hxx"

namespace replxx {

int mk_wcwidth( char32_t );

/**
 * Recompute widths of all characters in a char32_t buffer
 * @param text      - input buffer of Unicode characters
 * @param widths    - output buffer of character widths
 * @param charCount - number of characters in buffer
 */
void recompute_character_widths( char32_t const* text, char* widths, int charCount ) {
	for (int i = 0; i < charCount; ++i) {
		widths[i] = mk_wcwidth(text[i]);
	}
}

/**
 * Calculate a new screen position given a starting position, screen width and
 * character count
 * @param x             - initial x position (zero-based)
 * @param y             - initial y position (zero-based)
 * @param screenColumns - screen column count
 * @param charCount     - character positions to advance
 * @param xOut          - returned x position (zero-based)
 * @param yOut          - returned y position (zero-based)
 */
void calculate_screen_position(
	int x, int y, int screenColumns,
	int charCount, int& xOut, int& yOut
) {
	xOut = x;
	yOut = y;
	int charsRemaining = charCount;
	while ( charsRemaining > 0 ) {
		int charsThisRow = ( ( x + charsRemaining ) < screenColumns )
			? charsRemaining
			: screenColumns - x;
		xOut = x + charsThisRow;
		yOut = y;
		charsRemaining -= charsThisRow;
		x = 0;
		++ y;
	}
	if ( xOut == screenColumns ) {	// we have to special-case line wrap
		xOut = 0;
		++ yOut;
	}
}

/**
 * Calculate a column width using mk_wcswidth()
 * @param buf32 - text to calculate
 * @param len   - length of text to calculate
 */
int calculate_displayed_length( char32_t const* buf32_, int size_ ) {
	int len( 0 );
	for ( int i( 0 ); i < size_; ++ i ) {
		char32_t c( buf32_[i] );
		if ( c == '\033' ) {
			int escStart( i );
			++ i;
			if ( ( i < size_ ) && ( buf32_[i] != '[' ) ) {
				i = escStart;
				++ len;
				continue;
			}
			++ i;
			for ( ; i < size_; ++ i ) {
				c = buf32_[i];
				if ( ( c != ';' ) && ( ( c < '0' ) || ( c > '9' ) ) ) {
					break;
				}
			}
			if ( ( i < size_ ) && ( buf32_[i] == 'm' ) ) {
				continue;
			}
			i = escStart;
			len += 2;
		} else if ( is_control_code( c ) ) {
			len += 2;
		} else {
			int wcw( mk_wcwidth( c ) );
			if ( wcw < 0 ) {
				len = -1;
				break;
			}
			len += wcw;
		}
	}
	return ( len );
}

char const* ansi_color( Replxx::Color color_ ) {
	static char const reset[] = "\033[0m";
	static char const black[] = "\033[0;22;30m";
	static char const red[] = "\033[0;22;31m";
	static char const green[] = "\033[0;22;32m";
	static char const brown[] = "\033[0;22;33m";
	static char const blue[] = "\033[0;22;34m";
	static char const magenta[] = "\033[0;22;35m";
	static char const cyan[] = "\033[0;22;36m";
	static char const lightgray[] = "\033[0;22;37m";

#ifdef _WIN32
	static bool const has256colorDefault( true );
#else
	static bool const has256colorDefault( false );
#endif
	static char const* TERM( getenv( "TERM" ) );
	static bool const has256color( TERM ? ( strstr( TERM, "256" ) != nullptr ) : has256colorDefault );
	static char const* gray = has256color ? "\033[0;1;90m" : "\033[0;1;30m";
	static char const* brightred = has256color ? "\033[0;1;91m" : "\033[0;1;31m";
	static char const* brightgreen = has256color ? "\033[0;1;92m" : "\033[0;1;32m";
	static char const* yellow = has256color ? "\033[0;1;93m" : "\033[0;1;33m";
	static char const* brightblue = has256color ? "\033[0;1;94m" : "\033[0;1;34m";
	static char const* brightmagenta = has256color ? "\033[0;1;95m" : "\033[0;1;35m";
	static char const* brightcyan = has256color ? "\033[0;1;96m" : "\033[0;1;36m";
	static char const* white = has256color ? "\033[0;1;97m" : "\033[0;1;37m";
	static char const error[] = "\033[101;1;33m";

	char const* code( reset );
	switch ( color_ ) {
		case Replxx::Color::BLACK:         code = black;         break;
		case Replxx::Color::RED:           code = red;           break;
		case Replxx::Color::GREEN:         code = green;         break;
		case Replxx::Color::BROWN:         code = brown;         break;
		case Replxx::Color::BLUE:          code = blue;          break;
		case Replxx::Color::MAGENTA:       code = magenta;       break;
		case Replxx::Color::CYAN:          code = cyan;          break;
		case Replxx::Color::LIGHTGRAY:     code = lightgray;     break;
		case Replxx::Color::GRAY:          code = gray;          break;
		case Replxx::Color::BRIGHTRED:     code = brightred;     break;
		case Replxx::Color::BRIGHTGREEN:   code = brightgreen;   break;
		case Replxx::Color::YELLOW:        code = yellow;        break;
		case Replxx::Color::BRIGHTBLUE:    code = brightblue;    break;
		case Replxx::Color::BRIGHTMAGENTA: code = brightmagenta; break;
		case Replxx::Color::BRIGHTCYAN:    code = brightcyan;    break;
		case Replxx::Color::WHITE:         code = white;         break;
		case Replxx::Color::ERROR:         code = error;         break;
		case Replxx::Color::DEFAULT:       code = reset;         break;
	}
	return ( code );
}

std::string now_ms_str( void ) {
	std::chrono::milliseconds ms( std::chrono::duration_cast<std::chrono::milliseconds>( std::chrono::system_clock::now().time_since_epoch() ) );
	time_t t( ms.count() / 1000 );
	tm broken;
#ifdef _WIN32
#define localtime_r( t, b ) localtime_s( ( b ), ( t ) )
#endif
	localtime_r( &t, &broken );
#undef localtime_r
	static int const BUFF_SIZE( 32 );
	char str[BUFF_SIZE];
	strftime( str, BUFF_SIZE, "%Y-%m-%d %H:%M:%S.", &broken );
	snprintf( str + sizeof ( "YYYY-mm-dd HH:MM:SS" ), 5, "%03d", static_cast<int>( ms.count() % 1000 ) );
	return ( str );
}

}

