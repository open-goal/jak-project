#! /usr/bin/python3

import pexpect
import unittest
import re
import os
import subprocess
import signal
import time

keytab = {
	"<home>": "\033[1~",
	"<s-home>": "\033[1;2H",
	"<c-home>": "\033[1;5H",
	"<end>": "\033[4~",
	"<s-end>": "\033[1;2F",
	"<c-end>": "\033[1;5F",
	"<ins>": "\033[2~",
	"<s-ins>": "\033[2;2~",
	"<c-ins>": "\033[2;5~",
	"<del>": "\033[3~",
	"<s-del>": "\033[3;2~",
	"<c-del>": "\033[3;5~",
	"<pgup>": "\033[5~",
	"<c-pgup>": "\033[5;5~",
	"<pgdown>": "\033[6~",
	"<c-pgdown>": "\033[6;5~",
	"<backspace>": "",
	"<tab>": "\t",
	"<cr>": "\r",
	"<lf>": "\n",
	"<left>": "\033[D",
	"<s-left>": "\033[1;2D",
	"<aleft>": "\033OD",
	"<right>": "\033[C",
	"<s-right>": "\033[1;2C",
	"<aright>": "\033OC",
	"<up>": "\033[A",
	"<s-up>": "\033[1;2A",
	"<aup>": "\033OA",
	"<down>": "\033[B",
	"<s-down>": "\033[1;2B",
	"<adown>": "\033OB",
	"<c-left>": "\033[1;5D",
	"<c-right>": "\033[1;5C",
	"<c-up>": "\033[1;5A",
	"<c-down>": "\033[1;5B",
	"<m-left>": "\033[1;3D",
	"<m-right>": "\033[1;3C",
	"<m-up>": "\033[1;3A",
	"<m-down>": "\033[1;3B",
	"<c-a>": "",
	"<c-b>": "",
	"<c-c>": "",
	"<c-d>": "",
	"<c-e>": "",
	"<c-f>": "",
	"<c-k>": "",
	"<c-l>": "",
	"<c-n>": "",
	"<c-p>": "",
	"<c-r>": "",
	"<c-s>": "",
	"<c-t>": "",
	"<c-u>": "",
	"<c-v>": "",
	"<c-w>": "",
	"<c-y>": "",
	"<c-z>": "",
	"<m-b>": "\033b",
	"<m-c>": "\033c",
	"<m-d>": "\033d",
	"<m-f>": "\033f",
	"<m-l>": "\033l",
	"<m-n>": "\033n",
	"<m-p>": "\033p",
	"<m-u>": "\033u",
	"<m-y>": "\033y",
	"<m-.>": "\033.",
	"<m-backspace>": "\033\177",
	"<f1>": "\033OP",
	"<f2>": "\033OQ",
	"<f3>": "\033OR",
	"<f4>": "\033OS",
	"<f5>": "\033[15~",
	"<f6>": "\033[17~",
	"<f7>": "\033[18~",
	"<f8>": "\033[19~",
	"<f9>": "\033[20~",
	"<f10>": "\033[21~",
	"<f11>": "\033[23~",
	"<f12>": "\033[24~",
	"<s-f1>": "\033[1;2P",
	"<s-f2>": "\033[1;2Q",
	"<s-f3>": "\033[1;2R",
	"<s-f4>": "\033[1;2S",
	"<s-f5>": "\033[15;2~",
	"<s-f6>": "\033[17;2~",
	"<s-f7>": "\033[18;2~",
	"<s-f8>": "\033[19;2~",
	"<s-f9>": "\033[20;2~",
	"<s-f10>": "\033[21;2~",
	"<s-f11>": "\033[23;2~",
	"<s-f12>": "\033[24;2~",
	"<c-f1>": "\033[1;5P",
	"<c-f2>": "\033[1;5Q",
	"<c-f3>": "\033[1;5R",
	"<c-f4>": "\033[1;5S",
	"<c-f5>": "\033[15;5~",
	"<c-f6>": "\033[17;5~",
	"<c-f7>": "\033[18;5~",
	"<c-f8>": "\033[19;5~",
	"<c-f9>": "\033[20;5~",
	"<c-f10>": "\033[21;5~",
	"<c-f11>": "\033[23;5~",
	"<c-f12>": "\033[24;5~",
	"<s-tab>": "\033[Z",
	"<paste-pfx>": "\033[200~",
	"<paste-sfx>": "\033[201~"
}

termseq = {
	"\x1bc": "<RIS>",
	"\x1b[0m": "<rst>",
	"\x1b[H": "<mvhm>",
	"\x1b[2J": "<clr>",
	"\x1b[J": "<ceos>",
	"\x1b[0;22;30m": "<black>",
	"\x1b[0;22;31m": "<red>",
	"\x1b[0;22;32m": "<green>",
	"\x1b[0;22;33m": "<brown>",
	"\x1b[0;22;34m": "<blue>",
	"\x1b[0;22;35m": "<magenta>",
	"\x1b[0;22;36m": "<cyan>",
	"\x1b[0;22;37m": "<lightgray>",
	"\x1b[0;1;30m": "<gray>",
	"\x1b[0;1;31m": "<brightred>",
	"\x1b[0;1;32m": "<brightgreen>",
	"\x1b[0;1;33m": "<yellow>",
	"\x1b[0;1;34m": "<brightblue>",
	"\x1b[0;1;35m": "<brightmagenta>",
	"\x1b[0;1;36m": "<brightcyan>",
	"\x1b[0;1;37m": "<white>",
	"\x1b[1;32m": "<brightgreen>",
	"\x1b[101;1;33m": "<err>",
	"\x07": "<bell>",
	"\x1b[2~": "<ins-key>",
	"\x1b[?2004h": "<paste-on>",
	"\x1b[?2004l": "<paste-off>"
}
colRe = re.compile( "\\x1b\\[(\\d+)G" )
upRe = re.compile( "\\x1b\\[(\\d+)A" )
downRe = re.compile( "\\x1b\\[(\\d+)B" )

def sym_to_raw( str_ ):
	for sym, seq in keytab.items():
		if isinstance( str_, Rapid ):
			str_ = Rapid( str_.replace( sym, seq ) )
		else:
			str_ = str_.replace( sym, seq )
	return str_

def seq_to_sym( str_ ):
	for seq, sym in termseq.items():
		str_ = str_.replace( seq, sym )
	str_ = colRe.sub( "<c\\1>", str_ )
	str_ = upRe.sub( "<u\\1>", str_ )
	str_ = downRe.sub( "<d\\1>", str_ )
	return str_

_words_ = [
	"ada", "algol"
	"bash", "basic",
	"clojure", "cobol", "csharp",
	"eiffel", "erlang",
	"forth", "fortran", "fsharp",
	"go", "groovy",
	"haskell", "huginn",
	"java", "javascript", "julia",
	"kotlin",
	"lisp", "lua",
	"modula",
	"nemerle",
	"ocaml",
	"perl", "php", "prolog", "python",
	"rebol", "ruby", "rust",
	"scala", "scheme", "sql", "swift",
	"typescript"
]

def skip( test_ ):
	return "SKIP" in os.environ and os.environ["SKIP"].find( test_ ) >= 0

verbosity = None

class Rapid( str ): pass

def rapid( item ):
	if isinstance( item, str ):
		r = Rapid( item )
		return r
	return list( map( Rapid, item ) )

class ReplxxTests( unittest.TestCase ):
	_prompt_ = "\033\\[1;32mreplxx\033\\[0m> "
	_cxxSample_ = "./build/debug/replxx-example-cxx-api"
	_cSample_ = "./build/debug/replxx-example-c-api"
	_end_ = "\r\nExiting Replxx\r\n"
	def send_str( self_, str_, intraKeyDelay_ ):
		if isinstance(str_, Rapid):
			self_._replxx.send( str_ )
			return
		for char in str_:
			self_._replxx.send( char )
			time.sleep( intraKeyDelay_ )

	def check_scenario(
		self_, seq_, expected_,
		history = "one\ntwo\nthree\n",
		term = "xterm",
		command = _cxxSample_,
		dimensions = ( 25, 80 ),
		prompt = _prompt_,
		end = _prompt_ + _end_,
		encoding = "utf-8",
		pause = 0.25,
		intraKeyDelay = 0.002
	):
		with open( "replxx_history.txt", "wb" ) as f:
			f.write( history.encode( encoding ) )
			f.close()
		os.environ["TERM"] = term
		if isinstance( command, str ):
			command = command.replace( "\n", "~" )
		if verbosity >= 2:
			print( "\nTERM: {}, SIZE: {}, CMD: {}".format( term, dimensions, command ) )
		prompt = prompt.replace( "\n", "\r\n" ).replace( "\r\r", "\r" )
		end = end.replace( "\n", "\r\n" ).replace( "\r\r", "\r" )
		if isinstance( command, str ):
			self_._replxx = pexpect.spawn( command, maxread = 1, encoding = encoding, dimensions = dimensions )
		else:
			self_._replxx = pexpect.spawn( command[0], args = command[1:], maxread = 1, encoding = encoding, dimensions = dimensions )
		self_._replxx.expect( prompt )
		self_.maxDiff = None
		if isinstance( seq_, str ):
			if isinstance( seq_, Rapid ):
				seqs = rapid( seq_.split( "<c-z>" ) )
			else:
				seqs = seq_.split( "<c-z>" )
			for seq in seqs:
				last = seq is seqs[-1]
				if not last:
					seq += "<c-z>"
				self_.send_str( sym_to_raw( seq ), intraKeyDelay )
				if not last:
					time.sleep( pause )
					self_._replxx.kill( signal.SIGCONT )
		else:
			for seq in seq_:
				last = seq is seq_[-1]
				self_.send_str( sym_to_raw( seq ), intraKeyDelay )
				if not last:
					time.sleep( pause )
		self_._replxx.expect( end )
		if isinstance( expected_, str ):
			self_.assertSequenceEqual( seq_to_sym( self_._replxx.before ), expected_ )
		else:
			try:
				self_.assertIn( seq_to_sym( self_._replxx.before ), expected_ )
			except:
				self_.assertSequenceEqual( seq_to_sym( self_._replxx.before ), "" )
	def test_unicode( self_ ):
		self_.check_scenario(
			"<up><cr><c-d>",
			"<c9>aóą Ϩ 𓢀  󃔀  <rst><ceos><c21>"
			"<c9>aóą Ϩ 𓢀  󃔀  <rst><ceos><c21>\r\n"
			"aóą Ϩ 𓢀  󃔀  \r\n",
			"aóą Ϩ 𓢀  󃔀  \n"
		)
		self_.check_scenario(
			"aóą Ϩ 𓢀  󃔀  <cr><c-d>",
			"<c9>a<rst><ceos><c10><c9>aó<rst><ceos><c11><c9>aóą<rst><ceos><c12><c9>aóą "
			"<rst><ceos><c13><c9>aóą Ϩ<rst><ceos><c14><c9>aóą Ϩ "
			"<rst><ceos><c15><c9>aóą Ϩ 𓢀<rst><ceos><c16><c9>aóą Ϩ 𓢀 "
			"<rst><ceos><c17><c9>aóą Ϩ 𓢀  "
			"<rst><ceos><c18><c9>aóą Ϩ 𓢀  󃔀<rst><ceos><c19><c9>aóą Ϩ 𓢀  󃔀 "
			"<rst><ceos><c20><c9>aóą Ϩ 𓢀  󃔀  "
			"<rst><ceos><c21><c9>aóą Ϩ 𓢀  󃔀  <rst><ceos><c21>\r\n"
			"aóą Ϩ 𓢀  󃔀  \r\n"
		)
	@unittest.skipIf( skip( "8bit_encoding" ), "broken platform" )
	def test_8bit_encoding( self_ ):
		LC_CTYPE = "LC_CTYPE"
		exists = LC_CTYPE in os.environ
		lcCtype = None
		if exists:
			lcCtype = os.environ[LC_CTYPE]
		os.environ[LC_CTYPE] = "pl_PL.ISO-8859-2"
		self_.check_scenario(
			"<aup><cr><c-d>",
			"<c9>text ~ó~<rst><ceos><c17><c9>text ~ó~<rst><ceos><c17>\r\ntext ~ó~\r\n",
			"text ~ó~\n",
			encoding = "iso-8859-2"
		)
		if exists:
			os.environ[LC_CTYPE] = lcCtype
		else:
			del os.environ[LC_CTYPE]
	def test_bad_term( self_ ):
		self_.check_scenario(
			"a line of text<cr><c-d>",
			"a line of text\r\na line of text\r\n",
			term = "dumb"
		)
	def test_ctrl_c( self_ ):
		self_.check_scenario(
			"abc<c-c><c-d>",
			"<c9>a<rst><ceos><c10><c9>ab<rst><ceos><c11><c9>abc<rst><ceos><c12><c9>abc<rst><ceos><c12>^C\r"
			"\r\n"
		)
	def test_ctrl_z( self_ ):
		self_.check_scenario(
			"<up><c-z><cr><c-d>",
			"<c9>three<rst><ceos><c14><brightgreen>replxx<rst>> "
			"<c9>three<rst><ceos><c14><c9>three<rst><ceos><c14>\r\n"
			"three\r\n"
		)
		self_.check_scenario(
			"<c-r>w<c-z><cr><c-d>",
			"<c1><ceos><c1><ceos>(reverse-i-search)`': "
			"<c23><c1><ceos>(reverse-i-search)`w': "
			"two<c25><c1><ceos>(reverse-i-search)`w': "
			"two<c25><c1><ceos><brightgreen>replxx<rst>> "
			"two<c10><c9><ceos><c12>\r\n"
			"two\r\n"
		)
	def test_ctrl_l( self_ ):
		self_.check_scenario(
			"<cr><cr><cr><c-l><c-d>",
			"<c9><ceos><c9>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9>\r\n"
			"<brightgreen>replxx<rst>> <RIS><mvhm><clr><rst><brightgreen>replxx<rst>> "
			"<c9><rst><ceos><c9>",
			end = "\r\nExiting Replxx\r\n"
		)
		self_.check_scenario(
			"<cr><up><c-left><c-l><cr><c-d>",
			"<c9><ceos><c9>\r\n"
			"<brightgreen>replxx<rst>> <c9>first "
			"second<rst><ceos><c21><c9>first "
			"second<rst><ceos><c15><RIS><mvhm><clr><rst><brightgreen>replxx<rst>> "
			"<c9>first second<rst><ceos><c15><c9>first second<rst><ceos><c21>\r\n"
			"first second\r\n",
			"first second\n"
		)
	def test_backspace( self_ ):
		self_.check_scenario(
			"<up><c-a><m-f><c-right><backspace><backspace><backspace><backspace><cr><c-d>",
			"<c9>one two three<rst><ceos><c22><c9>one two "
			"three<rst><ceos><c9><c9>one two three<rst><ceos><c12><c9>one two "
			"three<rst><ceos><c16><c9>one tw three<rst><ceos><c15><c9>one t "
			"three<rst><ceos><c14><c9>one  three<rst><ceos><c13><c9>one "
			"three<rst><ceos><c12><c9>one three<rst><ceos><c18>\r\n"
			"one three\r\n",
			"one two three\n"
		)
	def test_delete( self_ ):
		self_.check_scenario(
			"<up><m-b><c-left><del><c-d><del><c-d><cr><c-d>",
			"<c9>one two three<rst><ceos><c22><c9>one two "
			"three<rst><ceos><c17><c9>one two three<rst><ceos><c13><c9>one wo "
			"three<rst><ceos><c13><c9>one o three<rst><ceos><c13><c9>one  "
			"three<rst><ceos><c13><c9>one three<rst><ceos><c13><c9>one three<rst><ceos><c18>\r\n"
			"one three\r\n",
			"one two three\n"
		)
	def test_home_key( self_ ):
		self_.check_scenario(
			"abc<home>z<cr><c-d>",
			"<c9>a<rst><ceos><c10><c9>ab<rst><ceos><c11><c9>abc<rst><ceos><c12><c9>abc<rst><ceos><c9><c9>zabc<rst><ceos><c10><c9>zabc<rst><ceos><c13>\r\n"
			"zabc\r\n"
		)
	def test_end_key( self_ ):
		self_.check_scenario(
			"abc<home>z<end>q<cr><c-d>",
			"<c9>a<rst><ceos><c10><c9>ab<rst><ceos><c11><c9>abc<rst><ceos><c12><c9>abc<rst><ceos><c9><c9>zabc<rst><ceos><c10><c9>zabc<rst><ceos><c13><c9>zabcq<rst><ceos><c14><c9>zabcq<rst><ceos><c14>\r\n"
			"zabcq\r\n"
		)
	def test_left_key( self_ ):
		self_.check_scenario(
			"abc<left>x<aleft><left>y<cr><c-d>",
			"<c9>a<rst><ceos><c10><c9>ab<rst><ceos><c11><c9>abc<rst><ceos><c12><c9>abc<rst><ceos><c11><c9>abxc<rst><ceos><c12><c9>abxc<rst><ceos><c11><c9>abxc<rst><ceos><c10><c9>aybxc<rst><ceos><c11><c9>aybxc<rst><ceos><c14>\r\n"
			"aybxc\r\n"
		)
	def test_right_key( self_ ):
		self_.check_scenario(
			"abc<home><right>x<aright>y<cr><c-d>",
			"<c9>a<rst><ceos><c10><c9>ab<rst><ceos><c11><c9>abc<rst><ceos><c12><c9>abc<rst><ceos><c9><c9>abc<rst><ceos><c10><c9>axbc<rst><ceos><c11><c9>axbc<rst><ceos><c12><c9>axbyc<rst><ceos><c13><c9>axbyc<rst><ceos><c14>\r\n"
			"axbyc\r\n"
		)
	def test_prev_word_key( self_ ):
		self_.check_scenario(
			"abc def ghi<c-left><m-left>x<cr><c-d>",
			"<c9>a<rst><ceos><c10><c9>ab<rst><ceos><c11><c9>abc<rst><ceos><c12><c9>abc "
			"<rst><ceos><c13><c9>abc d<rst><ceos><c14><c9>abc "
			"de<rst><ceos><c15><c9>abc def<rst><ceos><c16><c9>abc "
			"def <rst><ceos><c17><c9>abc def "
			"g<rst><ceos><c18><c9>abc def gh<rst><ceos><c19><c9>abc "
			"def ghi<rst><ceos><c20><c9>abc def ghi<rst><ceos><c17><c9>abc def "
			"ghi<rst><ceos><c13><c9>abc xdef ghi<rst><ceos><c14><c9>abc xdef "
			"ghi<rst><ceos><c21>\r\n"
			"abc xdef ghi\r\n"
		)
	def test_next_word_key( self_ ):
		self_.check_scenario(
			"abc def ghi<home><c-right><m-right>x<cr><c-d>",
			"<c9>a<rst><ceos><c10><c9>ab<rst><ceos><c11><c9>abc<rst><ceos><c12><c9>abc "
			"<rst><ceos><c13><c9>abc d<rst><ceos><c14><c9>abc "
			"de<rst><ceos><c15><c9>abc def<rst><ceos><c16><c9>abc "
			"def <rst><ceos><c17><c9>abc def "
			"g<rst><ceos><c18><c9>abc def gh<rst><ceos><c19><c9>abc "
			"def ghi<rst><ceos><c20><c9>abc def ghi<rst><ceos><c9><c9>abc def "
			"ghi<rst><ceos><c12><c9>abc def ghi<rst><ceos><c16><c9>abc defx "
			"ghi<rst><ceos><c17><c9>abc defx ghi<rst><ceos><c21>\r\n"
			"abc defx ghi\r\n"
		)
	def test_hint_show( self_ ):
		self_.check_scenario(
			"co\r<c-d>",
			"<c9>c<rst><ceos><c10><c9>co<rst><ceos>\r\n"
			"        <gray>color_black<rst>\r\n"
			"        <gray>color_red<rst>\r\n"
			"        <gray>color_green<rst><u3><c11><c9>co<rst><ceos><c11>\r\n"
			"co\r\n"
		)
		self_.check_scenario(
			"<up><cr><c-d>",
			"<c9>zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz "
			"<brightgreen>color_brightgreen<rst><ceos><c15><u3><c9>zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz "
			"<brightgreen>color_brightgreen<rst><ceos><c15>\r\n"
			"zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz color_brightgreen\r\n",
			"zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz color_brightgreen\n",
			dimensions = ( 64, 16 )
		)
	def test_hint_scroll_down( self_ ):
		self_.check_scenario(
			"co<c-down><c-down><tab><cr><c-d>",
			"<c9>c<rst><ceos><c10><c9>co<rst><ceos>\r\n"
			"        <gray>color_black<rst>\r\n"
			"        <gray>color_red<rst>\r\n"
			"        "
			"<gray>color_green<rst><u3><c11><c9>co<rst><ceos><gray>lor_black<rst>\r\n"
			"        <gray>color_red<rst>\r\n"
			"        <gray>color_green<rst>\r\n"
			"        "
			"<gray>color_brown<rst><u3><c11><c9>co<rst><ceos><gray>lor_red<rst>\r\n"
			"        <gray>color_green<rst>\r\n"
			"        <gray>color_brown<rst>\r\n"
			"        "
			"<gray>color_blue<rst><u3><c11><c9><red>color_red<rst><ceos><c18><c9><red>color_red<rst><ceos><c18>\r\n"
			"color_red\r\n"
		)
	def test_hint_scroll_up( self_ ):
		self_.check_scenario(
			"co<c-up><c-up><tab><cr><c-d>",
			"<c9>c<rst><ceos><c10><c9>co<rst><ceos>\r\n"
			"        <gray>color_black<rst>\r\n"
			"        <gray>color_red<rst>\r\n"
			"        "
			"<gray>color_green<rst><u3><c11><c9>co<rst><ceos><gray>lor_normal<rst>\r\n"
			"        <gray>co\r\n"
			"        <gray>color_black<rst>\r\n"
			"        "
			"<gray>color_red<rst><u3><c11><c9>co<rst><ceos><gray>lor_white<rst>\r\n"
			"        <gray>color_normal<rst>\r\n"
			"        <gray>co\r\n"
			"        "
			"<gray>color_black<rst><u3><c11><c9><white>color_white<rst><ceos><c20><c9><white>color_white<rst><ceos><c20>\r\n"
			"color_white\r\n"
		)
	def test_history( self_ ):
		self_.check_scenario(
			"<up><up><up><up><down><down><down><down>four<cr><c-d>",
			"<c9>three<rst><ceos><c14><c9>two<rst><ceos><c12><c9>one<rst><ceos><c12><c9>two<rst><ceos><c12><c9>three<rst><ceos><c14><c9><rst><ceos><c9><c9>f<rst><ceos><c10><c9>fo<rst><ceos><c11><c9>fou<rst><ceos><c12><c9>four<rst><ceos><c13><c9>four<rst><ceos><c13>\r\n"
			"four\r\n"
		)
		with open( "replxx_history.txt", "rb" ) as f:
			data = f.read().decode()
			self_.assertSequenceEqual( data[:-33], "### 0000-00-00 00:00:00.000\none\n### 0000-00-00 00:00:00.000\ntwo\n### 0000-00-00 00:00:00.000\nthree\n" )
			self_.assertSequenceEqual( data[-5:], "four\n" )
	def test_paren_matching( self_ ):
		self_.check_scenario(
			"ab(cd)ef<left><left><left><left><left><left><left><cr><c-d>",
			"<c9>a<rst><ceos><c10><c9>ab<rst><ceos><c11><c9>ab<brightmagenta>(<rst><ceos><c12><c9>ab<brightmagenta>(<rst>c<rst><ceos><c13><c9>ab<brightmagenta>(<rst>cd<rst><ceos><c14><c9>ab<brightmagenta>(<rst>cd<brightmagenta>)<rst><ceos><c15><c9>ab<brightmagenta>(<rst>cd<brightmagenta>)<rst>e<rst><ceos><c16><c9>ab<brightmagenta>(<rst>cd<brightmagenta>)<rst>ef<rst><ceos><c17><c9>ab<brightmagenta>(<rst>cd<brightmagenta>)<rst>ef<rst><ceos><c16><c9>ab<brightmagenta>(<rst>cd<brightmagenta>)<rst>ef<rst><ceos><c15><c9>ab<brightred>(<rst>cd<brightmagenta>)<rst>ef<rst><ceos><c14><c9>ab<brightmagenta>(<rst>cd<brightmagenta>)<rst>ef<rst><ceos><c13><c9>ab<brightmagenta>(<rst>cd<brightmagenta>)<rst>ef<rst><ceos><c12><c9>ab<brightmagenta>(<rst>cd<brightred>)<rst>ef<rst><ceos><c11><c9>ab<brightmagenta>(<rst>cd<brightmagenta>)<rst>ef<rst><ceos><c10><c9>ab<brightmagenta>(<rst>cd<brightmagenta>)<rst>ef<rst><ceos><c17>\r\n"
			"ab(cd)ef\r\n"
		)
	def test_paren_not_matched( self_ ):
		self_.check_scenario(
			"a(b[c)d<left><left><left><left><left><left><left><cr><c-d>",
			"<c9>a<rst><ceos><c10><c9>a<brightmagenta>(<rst><ceos><c11><c9>a<brightmagenta>(<rst>b<rst><ceos><c12><c9>a<brightmagenta>(<rst>b<brightmagenta>[<rst><ceos><c13><c9>a<brightmagenta>(<rst>b<brightmagenta>[<rst>c<rst><ceos><c14><c9>a<brightmagenta>(<rst>b<brightmagenta>[<rst>c<brightmagenta>)<rst><ceos><c15><c9>a<brightmagenta>(<rst>b<brightmagenta>[<rst>c<brightmagenta>)<rst>d<rst><ceos><c16><c9>a<brightmagenta>(<rst>b<brightmagenta>[<rst>c<brightmagenta>)<rst>d<rst><ceos><c15><c9>a<err>(<rst>b<brightmagenta>[<rst>c<brightmagenta>)<rst>d<rst><ceos><c14><c9>a<brightmagenta>(<rst>b<brightmagenta>[<rst>c<brightmagenta>)<rst>d<rst><ceos><c13><c9>a<brightmagenta>(<rst>b<brightmagenta>[<rst>c<brightmagenta>)<rst>d<rst><ceos><c12><c9>a<brightmagenta>(<rst>b<brightmagenta>[<rst>c<brightmagenta>)<rst>d<rst><ceos><c11><c9>a<brightmagenta>(<rst>b<brightmagenta>[<rst>c<err>)<rst>d<rst><ceos><c10><c9>a<brightmagenta>(<rst>b<brightmagenta>[<rst>c<brightmagenta>)<rst>d<rst><ceos><c9><c9>a<brightmagenta>(<rst>b<brightmagenta>[<rst>c<brightmagenta>)<rst>d<rst><ceos><c16>\r\n"
			"a(b[c)d\r\n"
		)
	def test_tab_completion( self_ ):
		self_.check_scenario(
			"co<tab><tab>bri<tab>b<tab><cr><c-d>",
			"<c9>c<rst><ceos><c10><c9>co<rst><ceos>\r\n"
			"        <gray>color_black<rst>\r\n"
			"        <gray>color_red<rst>\r\n"
			"        <gray>color_green<rst><u3><c11><c9>color_<rst><ceos>\r\n"
			"        <gray>color_black<rst>\r\n"
			"        <gray>color_red<rst>\r\n"
			"        <gray>color_green<rst><u3><c15><c9>color_<rst><ceos><c15>\r\n"
			"<brightmagenta>color_<rst>black          "
			"<brightmagenta>color_<rst>cyan           "
			"<brightmagenta>color_<rst>brightblue\r\n"
			"<brightmagenta>color_<rst><red>red<rst>            "
			"<brightmagenta>color_<rst>lightgray      "
			"<brightmagenta>color_<rst>brightmagenta\r\n"
			"<brightmagenta>color_<rst>green          "
			"<brightmagenta>color_<rst>gray           "
			"<brightmagenta>color_<rst>brightcyan\r\n"
			"<brightmagenta>color_<rst>brown          "
			"<brightmagenta>color_<rst><brightred>brightred<rst>      <brightmagenta>color_<rst>white\r\n"
			"<brightmagenta>color_<rst>blue           "
			"<brightmagenta>color_<rst>brightgreen    <brightmagenta>color_<rst>normal\r\n"
			"<brightmagenta>color_<rst>magenta        <brightmagenta>color_<rst>yellow\r\n"
			"<brightgreen>replxx<rst>> <c9>color_<rst><ceos>\r\n"
			"        <gray>color_black<rst>\r\n"
			"        <gray>color_red<rst>\r\n"
			"        <gray>color_green<rst><u3><c15><c9>color_b<rst><ceos>\r\n"
			"        <gray>color_black<rst>\r\n"
			"        <gray>color_brown<rst>\r\n"
			"        <gray>color_blue<rst><u3><c16><c9>color_br<rst><ceos>\r\n"
			"        <gray>color_brown<rst>\r\n"
			"        <gray>color_brightred<rst>\r\n"
			"        "
			"<gray>color_brightgreen<rst><u3><c17><c9>color_bri<rst><ceos>\r\n"
			"        <gray>color_brightred<rst>\r\n"
			"        <gray>color_brightgreen<rst>\r\n"
			"        "
			"<gray>color_brightblue<rst><u3><c18><c9>color_bright<rst><ceos>\r\n"
			"        <gray>color_brightred<rst>\r\n"
			"        <gray>color_brightgreen<rst>\r\n"
			"        "
			"<gray>color_brightblue<rst><u3><c21><c9>color_brightb<rst><ceos><green>lue<rst><c22><c9><brightblue>color_brightblue<rst><ceos><c25><c9><brightblue>color_brightblue<rst><ceos><c25>\r\n"
			"color_brightblue\r\n"
		)
		self_.check_scenario(
			"<tab><tab>n<cr><c-d>",
			"<bell><bell><c9>n<rst><ceos><c10><c9>n<rst><ceos><c10>\r\nn\r\n",
			dimensions = ( 4, 32 ),
			command = [ ReplxxTests._cSample_, "q1", "e0" ]
		)
		self_.check_scenario(
			"<tab><tab>n<cr><c-d>",
			"<c9><ceos><c9>\r\n"
			"db\r\n"
			"hello\r\n"
			"hallo\r\n"
			"--More--<bell>\r"
			"\t\t\t\t\r"
			"<brightgreen>replxx<rst>> "
			"<c9><rst><ceos><c9><c9><rst><ceos><c9>\r\n",
			dimensions = ( 4, 24 ),
			command = ReplxxTests._cSample_ + " q1 e1"
		)
		self_.check_scenario(
			"<up><home>co<tab><cr><c-d>",
			"<c9>abcd<brightmagenta>()<rst><ceos><c15>"
			"<c9>abcd<brightmagenta>()<rst><ceos><c9>"
			"<c9>cabcd<brightmagenta>()<rst><ceos><c10>"
			"<c9>coabcd<brightmagenta>()<rst><ceos><c11>"
			"<c9>color_abcd<brightmagenta>()<rst><ceos><c15>"
			"<c9>color_abcd<brightmagenta>()<rst><ceos><c21>\r\n"
			"color_abcd()\r\n",
			"abcd()\n"
		)
	def test_completion_shorter_result( self_ ):
		self_.check_scenario(
			"<up><tab><cr><c-d>",
			"<c9>\\pi<rst><ceos><c12><c9>π<rst><ceos><c10><c9>π<rst><ceos><c10>\r\n"
			"π\r\n",
			"\\pi\n"
		)
	def test_completion_pager( self_ ):
		cmd = ReplxxTests._cSample_ + " q1 x" + ",".join( _words_ )
		self_.check_scenario(
			"<tab>py<cr><c-d>",
			"<c9><ceos><c9>\r\n"
			"ada         groovy      perl\r\n"
			"algolbash   haskell     php\r\n"
			"basic       huginn      prolog\r\n"
			"clojure     java        python\r\n"
			"cobol       javascript  rebol\r\n"
			"csharp      julia       ruby\r\n"
			"eiffel      kotlin      rust\r\n"
			"erlang      lisp        scala\r\n"
			"forth       lua         scheme\r\n"
			"--More--<bell>\r"
			"\t\t\t\t\r"
			"fortran     modula      sql\r\n"
			"fsharp      nemerle     swift\r\n"
			"go          ocaml       typescript\r\n"
			"<brightgreen>replxx<rst>> "
			"<c9><rst><ceos><c9><c9><rst><ceos><c9>\r\n",
			dimensions = ( 10, 40 ),
			command = cmd
		)
		self_.check_scenario(
			"<tab><cr><cr><cr><cr><c-d>",
			"<c9><ceos><c9>\r\n"
			"ada         groovy      perl\r\n"
			"algolbash   haskell     php\r\n"
			"basic       huginn      prolog\r\n"
			"clojure     java        python\r\n"
			"cobol       javascript  rebol\r\n"
			"csharp      julia       ruby\r\n"
			"eiffel      kotlin      rust\r\n"
			"erlang      lisp        scala\r\n"
			"forth       lua         scheme\r\n"
			"--More--\r"
			"\t\t\t\t\r"
			"fortran     modula      sql\r\n"
			"--More--\r"
			"\t\t\t\t\r"
			"fsharp      nemerle     swift\r\n"
			"--More--\r"
			"\t\t\t\t\r"
			"go          ocaml       typescript\r\n"
			"<brightgreen>replxx<rst>> "
			"<c9><rst><ceos><c9><c9><rst><ceos><c9>\r\n",
			dimensions = ( 10, 40 ),
			command = cmd
		)
		self_.check_scenario(
			"<tab><c-c><cr><c-d>",
			"<c9><ceos><c9>\r\n"
			"ada         kotlin\r\n"
			"algolbash   lisp\r\n"
			"basic       lua\r\n"
			"clojure     modula\r\n"
			"cobol       nemerle\r\n"
			"csharp      ocaml\r\n"
			"eiffel      perl\r\n"
			"--More--^C\r\n"
			"<brightgreen>replxx<rst>> "
			"<c9><rst><ceos><c9><c9><rst><ceos><c9>\r\n",
			dimensions = ( 8, 32 ),
			command = cmd
		)
		self_.check_scenario(
			"<tab>q<cr><c-d>",
			"<c9><ceos><c9>\r\n"
			"ada         kotlin\r\n"
			"algolbash   lisp\r\n"
			"basic       lua\r\n"
			"clojure     modula\r\n"
			"cobol       nemerle\r\n"
			"csharp      ocaml\r\n"
			"eiffel      perl\r\n"
			"--More--\r"
			"\t\t\t\t\r"
			"<brightgreen>replxx<rst>> "
			"<c9><rst><ceos><c9><c9><rst><ceos><c9>\r\n",
			dimensions = ( 8, 32 ),
			command = cmd
		)
	def test_double_tab_completion( self_ ):
		cmd = ReplxxTests._cSample_ + " d1 q1 x" + ",".join( _words_ )
		self_.check_scenario(
			"fo<tab><tab>r<tab><cr><c-d>",
			"<c9>f<rst><ceos>\r\n"
			"        <gray>forth<rst>\r\n"
			"        <gray>fortran<rst>\r\n"
			"        <gray>fsharp<rst><u3><c10><c9>fo<rst><ceos>\r\n"
			"        <gray>forth<rst>\r\n"
			"        <gray>fortran<rst><u2><c11><c9>fort<rst><ceos>\r\n"
			"        <gray>forth<rst>\r\n"
			"        "
			"<gray>fortran<rst><u2><c13><c9>fortr<rst><ceos><gray>an<rst><c14><c9>fortran<rst><ceos><c16><c9>fortran<rst><ceos><c16>\r\n"
			"fortran\r\n",
			command = cmd
		)
	def test_beep_on_ambiguous_completion( self_ ):
		cmd = ReplxxTests._cSample_ + " b1 d1 q1 x" + ",".join( _words_ )
		self_.check_scenario(
			"fo<tab><tab>r<tab><cr><c-d>",
			"<c9>f<rst><ceos>\r\n"
			"        <gray>forth<rst>\r\n"
			"        <gray>fortran<rst>\r\n"
			"        <gray>fsharp<rst><u3><c10><c9>fo<rst><ceos>\r\n"
			"        <gray>forth<rst>\r\n"
			"        <gray>fortran<rst><u2><c11><bell><c9>fort<rst><ceos>\r\n"
			"        <gray>forth<rst>\r\n"
			"        "
			"<gray>fortran<rst><u2><c13><bell><c9>fortr<rst><ceos><gray>an<rst><c14><c9>fortran<rst><ceos><c16><c9>fortran<rst><ceos><c16>\r\n"
			"fortran\r\n",
			command = cmd
		)
	def test_history_search_backward( self_ ):
		self_.check_scenario(
			"<c-r>repl<c-r><cr><c-d>",
			"<c1><ceos><c1><ceos>(reverse-i-search)`': "
			"<c23><c1><ceos>(reverse-i-search)`r': echo repl "
			"golf<c29><c1><ceos>(reverse-i-search)`re': echo repl "
			"golf<c30><c1><ceos>(reverse-i-search)`rep': echo repl "
			"golf<c31><c1><ceos>(reverse-i-search)`repl': echo repl "
			"golf<c32><c1><ceos>(reverse-i-search)`repl': charlie repl "
			"delta<c35><c1><ceos><brightgreen>replxx<rst>> charlie repl "
			"delta<c17><c9><ceos><c27>\r\n"
			"charlie repl delta\r\n",
			"some command\n"
			"alfa repl bravo\n"
			"other request\n"
			"charlie repl delta\n"
			"misc input\n"
			"echo repl golf\n"
			"final thoughts\n"
		)
		self_.check_scenario(
			"<c-r>for<backspace><backspace>s<cr><c-d>",
			"<c1><ceos><c1><ceos>(reverse-i-search)`': "
			"<c23><c1><ceos>(reverse-i-search)`f': "
			"swift<c27><c1><ceos>(reverse-i-search)`fo': "
			"fortran<c25><c1><ceos>(reverse-i-search)`for': "
			"fortran<c26><c1><ceos>(reverse-i-search)`fo': "
			"fortran<c25><c1><ceos>(reverse-i-search)`f': "
			"swift<c27><c1><ceos>(reverse-i-search)`fs': "
			"fsharp<c25><c1><ceos><brightgreen>replxx<rst>> "
			"fsharp<c9><c9><ceos><c15>\r\n"
			"fsharp\r\n",
			"\n".join( _words_ ) + "\n"
		)
		self_.check_scenario(
			"<c-r>mod<c-l><cr><c-d>",
			"<c1><ceos><c1><ceos>(reverse-i-search)`': "
			"<c23><c1><ceos>(reverse-i-search)`m': "
			"scheme<c28><c1><ceos>(reverse-i-search)`mo': "
			"modula<c25><c1><ceos>(reverse-i-search)`mod': "
			"modula<c26><c1><ceos><brightgreen>replxx<rst>> "
			"<c9><RIS><mvhm><clr><rst><brightgreen>replxx<rst>> "
			"<c9><rst><ceos><c9><c9><rst><ceos><c9>\r\n",
			"\n".join( _words_ ) + "\n"
		)
	def test_history_search_forward( self_ ):
		self_.check_scenario(
			"<c-s>repl<c-s><cr><c-d>",
			"<c1><ceos><c1><ceos>(i-search)`': <c15><bell><c1><ceos>(i-search)`r': "
			"<c16><bell><c1><ceos>(i-search)`re': <c17><bell><c1><ceos>(i-search)`rep': "
			"<c18><bell><c1><ceos>(i-search)`repl': "
			"<c19><bell><c1><ceos>(i-search)`repl': "
			"<c19><c1><ceos><brightgreen>replxx<rst>> <c9><c9><ceos><c9>\r\n",
			"charlie repl delta\r\n",
			"some command\n"
			"alfa repl bravo\n"
			"other request\n"
			"charlie repl delta\n"
			"misc input\n"
			"echo repl golf\n"
			"final thoughts\n"
		)
		self_.check_scenario(
			"<pgup><c-s>repl<c-s><cr><c-d>",
			"<c9>final thoughts<rst><ceos><c23><c1><ceos><c1><ceos>(i-search)`': final "
			"thoughts<c29><c1><ceos>(i-search)`r': echo repl "
			"golf<c21><c1><ceos>(i-search)`re': echo repl "
			"golf<c22><c1><ceos>(i-search)`rep': echo repl "
			"golf<c23><c1><ceos>(i-search)`repl': echo repl "
			"golf<c24><c1><ceos>(i-search)`repl': alfa repl "
			"bravo<c24><c1><ceos><brightgreen>replxx<rst>> alfa repl bravo<c14><c9>final "
			"thoughts<rst><ceos><c24>\r\n"
			"alfa repl bravo\r\n",
			"final thoughts\n"
			"echo repl golf\n"
			"misc input\n"
			"charlie repl delta\n"
			"other request\n"
			"alfa repl bravo\n"
			"some command\n"
			"charlie repl delta\r\n",
		)
		self_.check_scenario(
			"<c-s>for<backspace><backspace>s<cr><c-d>",
			"<c1><ceos><c1><ceos>(i-search)`': <c15><bell><c1><ceos>(i-search)`f': "
			"<c16><bell><c1><ceos>(i-search)`fo': <c17><bell><c1><ceos>(i-search)`for': "
			"<c18><bell><c1><ceos>(i-search)`fo': <c17><bell><c1><ceos>(i-search)`f': "
			"<c16><bell><c1><ceos>(i-search)`fs': "
			"<c17><c1><ceos><brightgreen>replxx<rst>> <c9><c9><ceos><c9>\r\n",
			"\n".join( _words_[::-1] ) + "\n"
		)
		self_.check_scenario(
			"<pgup><c-s>for<backspace><backspace>s<cr><c-d>",
			"<c9>typescript<rst><ceos><c19><c1><ceos><c1><ceos>(i-search)`': "
			"typescript<c25><c1><ceos>(i-search)`f': swift<c19><c1><ceos>(i-search)`fo': "
			"fortran<c17><c1><ceos>(i-search)`for': fortran<c18><c1><ceos>(i-search)`fo': "
			"fortran<c17><c1><ceos>(i-search)`f': swift<c19><c1><ceos>(i-search)`fs': "
			"fsharp<c17><c1><ceos><brightgreen>replxx<rst>> "
			"fsharp<c9><c9>typescript<rst><ceos><c15>\r\n"
			"fsharp\r\n",
			"\n".join( _words_[::-1] ) + "\n"
		)
		self_.check_scenario(
			"<c-s>mod<c-l><cr><c-d>",
			"<c1><ceos><c1><ceos>(i-search)`': <c15><bell><c1><ceos>(i-search)`m': "
			"<c16><bell><c1><ceos>(i-search)`mo': <c17><bell><c1><ceos>(i-search)`mod': "
			"<c18><c1><ceos><brightgreen>replxx<rst>> "
			"<c9><RIS><mvhm><clr><rst><brightgreen>replxx<rst>> "
			"<c9><rst><ceos><c9><c9><rst><ceos><c9>\r\n",
			"\n".join( _words_[::-1] ) + "\n"
		)
		self_.check_scenario(
			"<pgup><c-s>mod<c-l><cr><c-d>",
			"<c9>typescript<rst><ceos><c19><c1><ceos><c1><ceos>(i-search)`': "
			"typescript<c25><c1><ceos>(i-search)`m': scheme<c20><c1><ceos>(i-search)`mo': "
			"modula<c17><c1><ceos>(i-search)`mod': "
			"modula<c18><c1><ceos><brightgreen>replxx<rst>> "
			"typescript<c19><RIS><mvhm><clr><rst><brightgreen>replxx<rst>> "
			"<c9>typescript<rst><ceos><c19><c9>typescript<rst><ceos><c19>\r\n"
			"typescript\r\n",
			"\n".join( _words_[::-1] ) + "\n"
		)
	def test_history_search_backward_position( self_ ):
		self_.check_scenario(
			"<c-r>req<up><cr><c-d>",
			"<c1><ceos><c1><ceos>(reverse-i-search)`': "
			"<c23><c1><ceos>(reverse-i-search)`r': echo repl "
			"golf<c29><c1><ceos>(reverse-i-search)`re': echo repl "
			"golf<c30><c1><ceos>(reverse-i-search)`req': other "
			"request<c32><c1><ceos><brightgreen>replxx<rst>> other request<c15><c9>alfa "
			"repl bravo<rst><ceos><c24><c9>alfa repl bravo<rst><ceos><c24>\r\n"
			"alfa repl bravo\r\n",
			"some command\n"
			"alfa repl bravo\n"
			"other request\n"
			"charlie repl delta\n"
			"misc input\n"
			"echo repl golf\n"
			"final thoughts\n"
		)
	def test_history_search_overlong_line( self_ ):
		self_.check_scenario(
			"<c-r>lo<cr><c-d>",
			"<c1><ceos><c1><ceos>(reverse-i-search)`': "
			"<c23><c1><ceos>(reverse-i-search)`l': some very long line of text, much "
			"longer then a witdth of a terminal, "
			"seriously<c37><u1><c1><ceos>(reverse-i-search)`lo': some very long line of "
			"text, much longer then a witdth of a terminal, "
			"seriously<u1><c59><c1><ceos><brightgreen>replxx<rst>> some very long line of "
			"text, much longer then a witdth of a terminal, "
			"seriously<u1><c43><c9><ceos><c24>\r\n"
			"some very long line of text, much longer then a witdth of a terminal, "
			"seriously\r\n",
			"fake\nsome very long line of text, much longer then a witdth of a terminal, seriously\nanother fake",
			dimensions = ( 24, 64 )
		)
	def test_history_prefix_search_backward( self_ ):
		self_.check_scenario(
			"repl<m-p><m-p><cr><c-d>",
			"<c9>r<rst><ceos><c10><c9>re<rst><ceos><c11><c9>rep<rst><ceos><c12><c9>repl<rst><ceos><c13><c9>repl_echo "
			"golf<rst><ceos><c23><c9>repl_charlie "
			"delta<rst><ceos><c27><c9>repl_charlie delta<rst><ceos><c27>\r\n"
			"repl_charlie delta\r\n",
			"some command\n"
			"repl_alfa bravo\n"
			"other request\n"
			"repl_charlie delta\n"
			"misc input\n"
			"repl_echo golf\n"
			"final thoughts\n"
		)
	def test_history_prefix_search_backward_position( self_ ):
		self_.check_scenario(
			"repl<m-p><up><cr><c-d>",
			"<c9>r<rst><ceos><c10><c9>re<rst><ceos><c11><c9>rep<rst><ceos><c12><c9>repl<rst><ceos><c13><c9>repl_echo "
			"golf<rst><ceos><c23><c9>misc input<rst><ceos><c19><c9>misc "
			"input<rst><ceos><c19>\r\n"
			"misc input\r\n",
			"some command\n"
			"repl_alfa bravo\n"
			"other request\n"
			"repl_charlie delta\n"
			"misc input\n"
			"repl_echo golf\n"
			"final thoughts\n"
		)
	def test_history_listing( self_ ):
		self_.check_scenario(
			"<up><cr><c-d>",
			"<c9><brightmagenta>.history<rst><ceos><c17><c9><brightmagenta>.history<rst><ceos><c17>\r\n"
			"   0: some command\r\n"
			"   1: repl_alfa bravo\r\n"
			"   2: other request\r\n"
			"   3: repl_charlie delta\r\n"
			"   4: misc input\r\n"
			"   5: repl_echo golf\r\n"
			"   6: .history\r\n",
			"some command\n"
			"repl_alfa bravo\n"
			"other request\n"
			"repl_charlie delta\n"
			"misc input\n"
			"repl_echo golf\n"
			".history\n"
		)
		self_.check_scenario(
			"<up><cr><c-d>",
			"<c9>/history<rst><ceos><c17><c9>/history<rst><ceos><c17>\r\n"
			"   0: some command\r\n"
			"   1: repl_alfa bravo\r\n"
			"   2: other request\r\n"
			"   3: repl_charlie delta\r\n"
			"   4: misc input\r\n"
			"   5: repl_echo golf\r\n"
			"   6: /history\r\n"
			"/history\r\n",
			"some command\n"
			"repl_alfa bravo\n"
			"other request\n"
			"repl_charlie delta\n"
			"misc input\n"
			"repl_echo golf\n"
			"/history\n",
			command = ReplxxTests._cSample_ + " q1"
		)
	def test_history_browse( self_ ):
		self_.check_scenario(
			"<up><aup><pgup><down><up><up><adown><pgdown><up><down><down><up><cr><c-d>",
			"<c9>twelve<rst><ceos><c15>"
			"<c9>eleven<rst><ceos><c15>"
			"<c9>one<rst><ceos><c12>"
			"<c9>two<rst><ceos><c12>"
			"<c9>one<rst><ceos><c12>"
			"<c9>two<rst><ceos><c12>"
			"<c9><rst><ceos><c9>"
			"<c9>twelve<rst><ceos><c15>"
			"<c9><rst><ceos><c9>"
			"<c9>twelve<rst><ceos><c15>"
			"<c9>twelve<rst><ceos><c15>\r\n"
			"twelve\r\n",
			"one\n"
			"two\n"
			"three\n"
			"four\n"
			"five\n"
			"six\n"
			"seven\n"
			"eight\n"
			"nine\n"
			"ten\n"
			"eleven\n"
			"twelve\n"
		)
	def test_history_max_size( self_ ):
		self_.check_scenario(
			"<pgup><pgdown>a<cr><pgup><cr><c-d>",
			"<c9>three<rst><ceos><c14><c9><rst><ceos><c9><c9>a<rst><ceos><c10><c9>a<rst><ceos><c10>\r\n"
			"a\r\n"
			"<brightgreen>replxx<rst>> "
			"<c9>four<rst><ceos><c13><c9>four<rst><ceos><c13>\r\n"
			"four\r\n",
			"one\n"
			"two\n"
			"three\n"
			"four\n"
			"five\n",
			command = ReplxxTests._cSample_ + " q1 s3"
		)
	def test_history_unique( self_ ):
		self_.check_scenario(
			"a<cr>b<cr>a<cr>b<cr><up><up><up><cr><c-d>",
			"<c9>a<rst><ceos><c10><c9>a<rst><ceos><c10>\r\n"
			"a\r\n"
			"<brightgreen>replxx<rst>> <c9>b<rst><ceos><c10><c9>b<rst><ceos><c10>\r\n"
			"b\r\n"
			"<brightgreen>replxx<rst>> <c9>a<rst><ceos><c10><c9>a<rst><ceos><c10>\r\n"
			"a\r\n"
			"<brightgreen>replxx<rst>> <c9>b<rst><ceos><c10><c9>b<rst><ceos><c10>\r\n"
			"b\r\n"
			"<brightgreen>replxx<rst>> "
			"<c9>b<rst><ceos><c10><c9>a<rst><ceos><c10><c9>c<rst><ceos><c10><c9>c<rst><ceos><c10>\r\n"
			"c\r\n",
			"a\nb\nc\n",
			command = ReplxxTests._cSample_ + " u1 q1"
		)
		self_.check_scenario(
			"a<cr>b<cr>a<cr>b<cr><up><up><up><cr><c-d>",
			"<c9>a<rst><ceos><c10><c9>a<rst><ceos><c10>\r\n"
			"a\r\n"
			"<brightgreen>replxx<rst>> <c9>b<rst><ceos><c10><c9>b<rst><ceos><c10>\r\n"
			"b\r\n"
			"<brightgreen>replxx<rst>> <c9>a<rst><ceos><c10><c9>a<rst><ceos><c10>\r\n"
			"a\r\n"
			"<brightgreen>replxx<rst>> <c9>b<rst><ceos><c10><c9>b<rst><ceos><c10>\r\n"
			"b\r\n"
			"<brightgreen>replxx<rst>> "
			"<c9>b<rst><ceos><c10><c9>a<rst><ceos><c10><c9>b<rst><ceos><c10><c9>b<rst><ceos><c10>\r\n"
			"b\r\n",
			"a\nb\nc\n",
			command = ReplxxTests._cSample_ + " u0 q1"
		)
		self_.check_scenario(
			rapid( "/history\n/unique\n/history\n<c-d>" ),
			"<c9>/<rst><ceos><c10><c9>/history<rst><ceos><c17>\r\n"
			"   0: a\r\n"
			"   1: b\r\n"
			"   2: c\r\n"
			"   3: b\r\n"
			"   4: c\r\n"
			"   5: d\r\n"
			"   6: a\r\n"
			"   7: c\r\n"
			"   8: c\r\n"
			"   9: a\r\n"
			"/history\r\n"
			"<brightgreen>replxx<rst>> <c9>/unique<rst><ceos><c16>\r\n"
			"/unique\r\n"
			"<brightgreen>replxx<rst>> <c9>/history<rst><ceos><c17>\r\n"
			"   0: b\r\n"
			"   1: d\r\n"
			"   2: c\r\n"
			"   3: a\r\n"
			"   4: /history\r\n"
			"   5: /unique\r\n"
			"/history\r\n",
			"a\nb\nc\nb\nc\nd\na\nc\nc\na\n",
			command = ReplxxTests._cSample_ + " u0 q1"
		)
	def test_history_recall_most_recent( self_ ):
		self_.check_scenario(
			"<pgup><down><cr><down><cr><c-d>",
			"<c9>aaaa<rst><ceos><c13><c9>bbbb<rst><ceos><c13><c9>bbbb<rst><ceos><c13>\r\n"
			"bbbb\r\n"
			"<brightgreen>replxx<rst>> "
			"<c9>cccc<rst><ceos><c13><c9>cccc<rst><ceos><c13>\r\n"
			"cccc\r\n",
			"aaaa\nbbbb\ncccc\ndddd\n"
		)
	def test_history_abort_incremental_history_search_position( self_ ):
		self_.check_scenario(
			"<up><up><c-r>cc<c-c><up><cr><c-d>",
			"<c9>hhhh<rst><ceos><c13><c9>gggg<rst><ceos><c13><c1><ceos><c1><ceos>(reverse-i-search)`': "
			"gggg<c27><c1><ceos>(reverse-i-search)`c': "
			"cccc<c27><c1><ceos>(reverse-i-search)`cc': "
			"cccc<c27><c1><ceos><brightgreen>replxx<rst>> "
			"gggg<c13><c9>gggg<rst><ceos><c13><c9>ffff<rst><ceos><c13><c9>ffff<rst><ceos><c13>\r\n"
			"ffff\r\n",
			"aaaa\nbbbb\ncccc\ndddd\neeee\nffff\ngggg\nhhhh\n"
		)
	def test_capitalize( self_ ):
		self_.check_scenario(
			"<up><home><right><m-c><m-c><right><right><m-c><m-c><m-c><cr><c-d>",
			"<c9>abc defg ijklmn zzxq<rst><ceos><c29><c9>abc defg ijklmn "
			"zzxq<rst><ceos><c9><c9>abc defg ijklmn zzxq<rst><ceos><c10><c9>aBc defg "
			"ijklmn zzxq<rst><ceos><c12><c9>aBc Defg ijklmn zzxq<rst><ceos><c17><c9>aBc "
			"Defg ijklmn zzxq<rst><ceos><c18><c9>aBc Defg ijklmn "
			"zzxq<rst><ceos><c19><c9>aBc Defg iJklmn zzxq<rst><ceos><c24><c9>aBc Defg "
			"iJklmn Zzxq<rst><ceos><c29><c9>aBc Defg iJklmn Zzxq<rst><ceos><c29>\r\n"
			"aBc Defg iJklmn Zzxq\r\n",
			"abc defg ijklmn zzxq\n"
		)
	def test_make_upper_case( self_ ):
		self_.check_scenario(
			"<up><home><right><right><right><m-u><m-u><right><m-u><cr><c-d>",
			"<c9>abcdefg hijklmno pqrstuvw<rst><ceos><c34><c9>abcdefg "
			"hijklmno pqrstuvw<rst><ceos><c9><c9>abcdefg hijklmno "
			"pqrstuvw<rst><ceos><c10><c9>abcdefg hijklmno "
			"pqrstuvw<rst><ceos><c11><c9>abcdefg hijklmno "
			"pqrstuvw<rst><ceos><c12><c9>abcDEFG hijklmno "
			"pqrstuvw<rst><ceos><c16><c9>abcDEFG HIJKLMNO "
			"pqrstuvw<rst><ceos><c25><c9>abcDEFG HIJKLMNO "
			"pqrstuvw<rst><ceos><c26><c9>abcDEFG HIJKLMNO "
			"PQRSTUVW<rst><ceos><c34><c9>abcDEFG HIJKLMNO "
			"PQRSTUVW<rst><ceos><c34>\r\n"
			"abcDEFG HIJKLMNO PQRSTUVW\r\n",
			"abcdefg hijklmno pqrstuvw\n"
		)
	def test_make_lower_case( self_ ):
		self_.check_scenario(
			"<up><home><right><right><right><m-l><m-l><right><m-l><cr><c-d>",
			"<c9>ABCDEFG HIJKLMNO PQRSTUVW<rst><ceos><c34><c9>ABCDEFG "
			"HIJKLMNO PQRSTUVW<rst><ceos><c9><c9>ABCDEFG HIJKLMNO "
			"PQRSTUVW<rst><ceos><c10><c9>ABCDEFG HIJKLMNO "
			"PQRSTUVW<rst><ceos><c11><c9>ABCDEFG HIJKLMNO "
			"PQRSTUVW<rst><ceos><c12><c9>ABCdefg HIJKLMNO "
			"PQRSTUVW<rst><ceos><c16><c9>ABCdefg hijklmno "
			"PQRSTUVW<rst><ceos><c25><c9>ABCdefg hijklmno "
			"PQRSTUVW<rst><ceos><c26><c9>ABCdefg hijklmno "
			"pqrstuvw<rst><ceos><c34><c9>ABCdefg hijklmno "
			"pqrstuvw<rst><ceos><c34>\r\n"
			"ABCdefg hijklmno pqrstuvw\r\n",
			"ABCDEFG HIJKLMNO PQRSTUVW\n"
		)
	def test_transpose( self_ ):
		self_.check_scenario(
			"<up><home><c-t><right><c-t><c-t><c-t><c-t><c-t><cr><c-d>",
			"<c9>abcd<rst><ceos><c13>"
			"<c9>abcd<rst><ceos><c9>"
			"<c9>abcd<rst><ceos><c10>"
			"<c9>bacd<rst><ceos><c11>"
			"<c9>bcad<rst><ceos><c12>"
			"<c9>bcda<rst><ceos><c13>"
			"<c9>bcad<rst><ceos><c13>"
			"<c9>bcda<rst><ceos><c13>"
			"<c9>bcda<rst><ceos><c13>\r\n"
			"bcda\r\n",
			"abcd\n"
		)
	def test_kill_to_beginning_of_line( self_ ):
		self_.check_scenario(
			"<up><home><c-right><c-right><right><c-u><end><c-y><cr><c-d>",
			"<c9><brightblue>+<rst>abc defg<brightblue>--<rst>ijklmn "
			"zzxq<brightblue>+<rst><ceos><c32><c9><brightblue>+<rst>abc "
			"defg<brightblue>--<rst>ijklmn "
			"zzxq<brightblue>+<rst><ceos><c9><c9><brightblue>+<rst>abc "
			"defg<brightblue>--<rst>ijklmn "
			"zzxq<brightblue>+<rst><ceos><c13><c9><brightblue>+<rst>abc "
			"defg<brightblue>--<rst>ijklmn "
			"zzxq<brightblue>+<rst><ceos><c18><c9><brightblue>+<rst>abc "
			"defg<brightblue>--<rst>ijklmn "
			"zzxq<brightblue>+<rst><ceos><c19><c9><brightblue>-<rst>ijklmn "
			"zzxq<brightblue>+<rst><ceos><c9><c9><brightblue>-<rst>ijklmn "
			"zzxq<brightblue>+<rst><ceos><c22><c9><brightblue>-<rst>ijklmn "
			"zzxq<brightblue>++<rst>abc "
			"defg<brightblue>-<rst><ceos><c32><c9><brightblue>-<rst>ijklmn "
			"zzxq<brightblue>++<rst>abc defg<brightblue>-<rst><ceos><c32>\r\n"
			"-ijklmn zzxq++abc defg-\r\n",
			"+abc defg--ijklmn zzxq+\n"
		)
	def test_kill_to_end_of_line( self_ ):
		self_.check_scenario(
			"<up><home><c-right><c-right><right><c-k><home><c-y><cr><c-d>",
			"<c9><brightblue>+<rst>abc defg<brightblue>--<rst>ijklmn "
			"zzxq<brightblue>+<rst><ceos><c32><c9><brightblue>+<rst>abc "
			"defg<brightblue>--<rst>ijklmn "
			"zzxq<brightblue>+<rst><ceos><c9><c9><brightblue>+<rst>abc "
			"defg<brightblue>--<rst>ijklmn "
			"zzxq<brightblue>+<rst><ceos><c13><c9><brightblue>+<rst>abc "
			"defg<brightblue>--<rst>ijklmn "
			"zzxq<brightblue>+<rst><ceos><c18><c9><brightblue>+<rst>abc "
			"defg<brightblue>--<rst>ijklmn "
			"zzxq<brightblue>+<rst><ceos><c19><c9><brightblue>+<rst>abc "
			"defg<brightblue>-<rst><ceos><c19><c9><brightblue>+<rst>abc "
			"defg<brightblue>-<rst><ceos><c9><c9><brightblue>-<rst>ijklmn "
			"zzxq<brightblue>++<rst>abc "
			"defg<brightblue>-<rst><ceos><c22><c9><brightblue>-<rst>ijklmn "
			"zzxq<brightblue>++<rst>abc defg<brightblue>-<rst><ceos><c32>\r\n"
			"-ijklmn zzxq++abc defg-\r\n",
			"+abc defg--ijklmn zzxq+\n"
		)
	def test_kill_next_word( self_ ):
		self_.check_scenario(
			"<up><home><c-right><m-d><c-right><c-y><cr><c-d>",
			"<c9>alpha charlie bravo delta<rst><ceos><c34><c9>alpha "
			"charlie bravo delta<rst><ceos><c9><c9>alpha charlie bravo "
			"delta<rst><ceos><c14><c9>alpha bravo delta<rst><ceos><c14><c9>alpha bravo "
			"delta<rst><ceos><c20><c9>alpha bravo charlie delta<rst><ceos><c28><c9>alpha "
			"bravo charlie delta<rst><ceos><c34>\r\n"
			"alpha bravo charlie delta\r\n",
			"alpha charlie bravo delta\n"
		)
	def test_kill_prev_word_to_white_space( self_ ):
		self_.check_scenario(
			"<up><c-left><c-w><c-left><c-y><cr><c-d>",
			"<c9>alpha charlie bravo delta<rst><ceos><c34><c9>alpha "
			"charlie bravo delta<rst><ceos><c29><c9>alpha charlie "
			"delta<rst><ceos><c23><c9>alpha charlie delta<rst><ceos><c15><c9>alpha bravo "
			"charlie delta<rst><ceos><c21><c9>alpha bravo charlie delta<rst><ceos><c34>\r\n"
			"alpha bravo charlie delta\r\n",
			"alpha charlie bravo delta\n"
		)
	def test_kill_prev_word( self_ ):
		self_.check_scenario(
			"<up><c-left><m-backspace><c-left><c-y><cr><c-d>",
			"<c9>alpha<brightmagenta>.<rst>charlie "
			"bravo<brightmagenta>.<rst>delta<rst><ceos><c34><c9>alpha<brightmagenta>.<rst>charlie "
			"bravo<brightmagenta>.<rst>delta<rst><ceos><c29><c9>alpha<brightmagenta>.<rst>charlie "
			"delta<rst><ceos><c23><c9>alpha<brightmagenta>.<rst>charlie "
			"delta<rst><ceos><c15><c9>alpha<brightmagenta>.<rst>bravo<brightmagenta>.<rst>charlie "
			"delta<rst><ceos><c21><c9>alpha<brightmagenta>.<rst>bravo<brightmagenta>.<rst>charlie "
			"delta<rst><ceos><c34>\r\n"
			"alpha.bravo.charlie delta\r\n",
			"alpha.charlie bravo.delta\n"
		)
	def test_kill_ring( self_ ):
		self_.check_scenario(
			"<up><c-w><backspace><c-w><backspace><c-w><backspace><c-u><c-y><m-y><m-y><m-y> <c-y><m-y><m-y><m-y> <c-y><m-y><m-y><m-y> <c-y><m-y><m-y><m-y><cr><c-d>",
			"<c9>delta charlie bravo alpha<rst><ceos><c34><c9>delta "
			"charlie bravo <rst><ceos><c29><c9>delta charlie "
			"bravo<rst><ceos><c28><c9>delta charlie "
			"<rst><ceos><c23><c9>delta "
			"charlie<rst><ceos><c22><c9>delta "
			"<rst><ceos><c15>"
			"<c9>delta<rst><ceos><c14>"
			"<c9><rst><ceos><c9>"
			"<c9>delta<rst><ceos><c14>"
			"<c9>charlie<rst><ceos><c16>"
			"<c9>bravo<rst><ceos><c14>"
			"<c9>alpha<rst><ceos><c14>"
			"<c9>alpha "
			"<rst><ceos><c15><c9>alpha "
			"alpha<rst><ceos><c20><c9>alpha "
			"delta<rst><ceos><c20><c9>alpha "
			"charlie<rst><ceos><c22><c9>alpha "
			"bravo<rst><ceos><c20><c9>alpha bravo "
			"<rst><ceos><c21><c9>alpha bravo "
			"bravo<rst><ceos><c26><c9>alpha bravo "
			"alpha<rst><ceos><c26><c9>alpha bravo "
			"delta<rst><ceos><c26><c9>alpha bravo "
			"charlie<rst><ceos><c28><c9>alpha bravo charlie "
			"<rst><ceos><c29><c9>alpha bravo charlie "
			"charlie<rst><ceos><c36><c9>alpha bravo charlie "
			"bravo<rst><ceos><c34><c9>alpha bravo charlie "
			"alpha<rst><ceos><c34><c9>alpha bravo charlie "
			"delta<rst><ceos><c34><c9>alpha bravo charlie delta<rst><ceos><c34>\r\n"
			"alpha bravo charlie delta\r\n",
			"delta charlie bravo alpha\n"
		)
		self_.check_scenario(
			"<up><c-w><c-w><backspace><c-a><c-y> <cr><c-d>",
			"<c9>charlie delta alpha bravo<rst><ceos><c34><c9>charlie "
			"delta alpha <rst><ceos><c29><c9>charlie delta "
			"<rst><ceos><c23><c9>charlie "
			"delta<rst><ceos><c22><c9>charlie delta<rst><ceos><c9><c9>alpha "
			"bravocharlie delta<rst><ceos><c20><c9>alpha bravo charlie "
			"delta<rst><ceos><c21><c9>alpha bravo charlie delta<rst><ceos><c34>\r\n"
			"alpha bravo charlie delta\r\n",
			"charlie delta alpha bravo\n"
		)
		self_.check_scenario(
			"<up><home><m-d><m-d><del><c-e> <c-y><cr><c-d>",
			"<c9>charlie delta alpha bravo<rst><ceos><c34><c9>charlie "
			"delta alpha bravo<rst><ceos><c9><c9> delta alpha bravo<rst><ceos><c9><c9> "
			"alpha bravo<rst><ceos><c9><c9>alpha bravo<rst><ceos><c9><c9>alpha "
			"bravo<rst><ceos><c20><c9>alpha bravo "
			"<rst><ceos><c21><c9>alpha bravo charlie "
			"delta<rst><ceos><c34><c9>alpha bravo charlie delta<rst><ceos><c34>\r\n"
			"alpha bravo charlie delta\r\n",
			"charlie delta alpha bravo\n"
		)
		self_.check_scenario(
			"<up><c-w><backspace><c-w><backspace><c-w><backspace><c-w><backspace><c-w><backspace>"
			"<c-w><backspace><c-w><backspace><c-w><backspace><c-w><backspace><c-w><backspace>"
			"<c-w><c-y><m-y><m-y><m-y><m-y><m-y><m-y><m-y><m-y><m-y><m-y><cr><c-d>",
			"<c9>a b c d e f g h i j k<rst><ceos><c30><c9>a b c d e f g "
			"h i j <rst><ceos><c29><c9>a b c d e f g h i "
			"j<rst><ceos><c28><c9>a b c d e f g h i "
			"<rst><ceos><c27><c9>a b c d e f g h "
			"i<rst><ceos><c26><c9>a b c d e f g h "
			"<rst><ceos><c25><c9>a b c d e f g "
			"h<rst><ceos><c24><c9>a b c d e f g "
			"<rst><ceos><c23><c9>a b c d e f g<rst><ceos><c22><c9>a "
			"b c d e f <rst><ceos><c21><c9>a b c d e "
			"f<rst><ceos><c20><c9>a b c d e <rst><ceos><c19><c9>a b "
			"c d e<rst><ceos><c18><c9>a b c d <rst><ceos><c17><c9>a "
			"b c d<rst><ceos><c16><c9>a b c <rst><ceos><c15><c9>a b "
			"c<rst><ceos><c14><c9>a b <rst><ceos><c13><c9>a "
			"b<rst><ceos><c12><c9>a "
			"<rst><ceos><c11>"
			"<c9>a<rst><ceos><c10>"
			"<c9><rst><ceos><c9>"
			"<c9>a<rst><ceos><c10>"
			"<c9>b<rst><ceos><c10>"
			"<c9>c<rst><ceos><c10>"
			"<c9>d<rst><ceos><c10>"
			"<c9>e<rst><ceos><c10>"
			"<c9>f<rst><ceos><c10>"
			"<c9>g<rst><ceos><c10>"
			"<c9>h<rst><ceos><c10>"
			"<c9>i<rst><ceos><c10>"
			"<c9>j<rst><ceos><c10>"
			"<c9>a<rst><ceos><c10>"
			"<c9>a<rst><ceos><c10>\r\n"
			"a\r\n",
			"a b c d e f g h i j k\n"
		)
	def test_yank_last_arg( self_ ):
		self_.check_scenario(
			"0123<left><left><m-.><m-.><m-.><cr><c-d>",
			"<c9><yellow>0<rst><ceos><c10>"
			"<c9><yellow>01<rst><ceos><c11>"
			"<c9><yellow>012<rst><ceos><c12>"
			"<c9><yellow>0123<rst><ceos><c13>"
			"<c9><yellow>0123<rst><ceos><c12>"
			"<c9><yellow>0123<rst><ceos><c11>"
			"<c9><yellow>01<rst>cat<yellow>23<rst><ceos><c14>"
			"<c9><yellow>01<rst>trillion<yellow>23<rst><ceos><c19>"
			"<c9><yellow>01<rst>twelve<yellow>23<rst><ceos><c17>"
			"<c9><yellow>01<rst>twelve<yellow>23<rst><ceos><c19>\r\n"
			"01twelve23\r\n",
			"one two three\nten eleven twelve\nmillion trillion\ndog cat\n"
		)
		self_.check_scenario(
			"<up><up><up> <m-.><m-.><cr><c-d>",
			"<c9>dog cat<rst><ceos><c16><c9>million trillion<rst><ceos><c25><c9>ten "
			"eleven twelve<rst><ceos><c26><c9>ten eleven twelve <rst><ceos><c27><c9>ten "
			"eleven twelve cat<rst><ceos><c30><c9>ten eleven twelve "
			"trillion<rst><ceos><c35><c9>ten eleven twelve trillion<rst><ceos><c35>\r\n"
			"ten eleven twelve trillion\r\n",
			"one two three\nten eleven twelve\nmillion trillion\ndog cat\n"
		)
	def test_tab_completion_cutoff( self_ ):
		self_.check_scenario(
			"<tab>n<tab>y<cr><c-d>",
			"<c9><rst><ceos><c9>\r\n"
			"Display all 9 possibilities? (y or n)\r\n"
			"<brightgreen>replxx<rst>> "
			"<c9><rst><ceos><c9><c9><rst><ceos><c9>\r\n"
			"Display all 9 possibilities? (y or n)<ceos>\r\n"
			"db            hallo         hansekogge    quetzalcoatl  power\r\n"
			"hello         hans          seamann       quit\r\n"
			"<brightgreen>replxx<rst>> "
			"<c9><rst><ceos><c9><c9><rst><ceos><c9>\r\n",
			command = ReplxxTests._cSample_ + " q1 c3"
		)
		self_.check_scenario(
			"<tab>n<cr><c-d>",
			"<c9><rst><ceos><c9>\r\n"
			"Display all 9 possibilities? (y or n)\r\n"
			"<brightgreen>replxx<rst>> "
			"<c9><rst><ceos><c9><c9><rst><ceos><c9>\r\n",
			command = ReplxxTests._cSample_ + " q1 c3"
		)
		self_.check_scenario(
			"<tab><c-c><cr><c-d>",
			"<c9><rst><ceos><c9>\r\n"
			"Display all 9 possibilities? (y or n)^C\r\n"
			"<brightgreen>replxx<rst>> "
			"<c9><rst><ceos><c9><c9><rst><ceos><c9>\r\n",
			command = ReplxxTests._cSample_ + " q1 c3"
		)
		self_.check_scenario(
			["<tab>", "<c-c><cr><c-d>"],
			"<c9><rst><ceos><c9>\r\n"
			"Display all 9 possibilities? (y or n)^C\r\n"
			"<brightgreen>replxx<rst>> "
			"<c9><rst><ceos><c9><c9><rst><ceos><c9>\r\n",
			command = ReplxxTests._cSample_ + " q1 c3 H200"
		)
	def test_preload( self_ ):
		self_.check_scenario(
			"<cr><c-d>",
			"<c9>Alice has a cat.<rst><ceos><c25>"
			"<c9>Alice has a cat.<rst><ceos><c25>\r\n"
			"Alice has a cat.\r\n",
			command = ReplxxTests._cSample_ + " q1 'iAlice has a cat.'"
		)
		self_.check_scenario(
			"<cr><c-d>",
			"<c9>Cat  eats  mice. "
			"<rst><ceos><c26><c9>Cat  eats  mice. "
			"<rst><ceos><c26>\r\n"
			"Cat  eats  mice. "
			"\r\n",
			command = ReplxxTests._cSample_ + " q1 'iCat\teats\tmice.\r\n'"
		)
		self_.check_scenario(
			"<cr><c-d>",
			"<c9>Cat  eats  mice. "
			"<rst><ceos><c26><c9>Cat  eats  mice. "
			"<rst><ceos><c26>\r\n"
			"Cat  eats  mice. "
			"\r\n",
			command = ReplxxTests._cSample_ + " q1 'iCat\teats\tmice.\r\n\r\n\n\n'"
		)
		self_.check_scenario(
			"<cr><c-d>",
			"<c9>M Alice has a cat.<rst><ceos><c27>"
			"<c9>M Alice has a cat.<rst><ceos><c27>\r\n"
			"M Alice has a cat.\r\n",
			command = ReplxxTests._cSample_ + " q1 'iMAlice has a cat.'"
		)
		self_.check_scenario(
			"<cr><c-d>",
			"<c9>M  Alice has a cat.<rst><ceos><c28>"
			"<c9>M  Alice has a cat.<rst><ceos><c28>\r\n"
			"M  Alice has a cat.\r\n",
			command = ReplxxTests._cSample_ + " q1 'iM\t\t\t\tAlice has a cat.'"
		)
	def test_prompt( self_ ):
		prompt = "date: now\nrepl> "
		self_.check_scenario(
			"<up><cr><up><up><cr><c-d>",
			"<c7>three<rst><ceos><c12><c7>three<rst><ceos><c12>\r\n"
			"three\r\n"
			"date: now\r\n"
			"repl> "
			"<c7>three<rst><ceos><c12><c7>two<rst><ceos><c10><c7>two<rst><ceos><c10>\r\n"
			"two\r\n",
			command = ReplxxTests._cSample_ + " q1 'p{}'".format( prompt ),
			prompt = prompt,
			end = prompt + ReplxxTests._end_
		)
		prompt = "repl>\n"
		self_.check_scenario(
			"a<cr><c-d>",
			"<c1>a<rst><ceos><c2><c1>a<rst><ceos><c2>\r\na\r\n",
			command = ReplxxTests._cSample_ + " q1 'p{}'".format( prompt ),
			prompt = prompt,
			end = prompt + ReplxxTests._end_
		)
	def test_long_line( self_ ):
		self_.check_scenario(
			"<up><c-left>~<c-left>~<c-left>~<c-left>~<c-left>~<c-left>~<c-left>~<c-left>~<c-left>~<c-left>~<c-left>~<c-left>~<c-left><cr><c-d>",
			"<c9>ada clojure eiffel fortran groovy java kotlin modula perl python "
			"rust sql<rst><ceos><c2><u2><c9>ada clojure eiffel fortran groovy "
			"java kotlin modula perl python rust sql<rst><ceos><u1><c39><u1><c9>ada "
			"clojure eiffel fortran groovy java kotlin modula perl python rust "
			"~sql<rst><ceos><u1><c40><u1><c9>ada clojure eiffel fortran groovy java "
			"kotlin modula perl python rust ~sql<rst><ceos><u1><c34><u1><c9>ada clojure "
			"eiffel fortran groovy java kotlin modula perl python ~rust "
			"~sql<rst><ceos><u1><c35><u1><c9>ada clojure eiffel fortran groovy java "
			"kotlin modula perl python ~rust ~sql<rst><ceos><u1><c27><u1><c9>ada clojure "
			"eiffel fortran groovy java kotlin modula perl ~python ~rust "
			"~sql<rst><ceos><u1><c28><u1><c9>ada clojure eiffel fortran groovy java "
			"kotlin modula perl ~python ~rust ~sql<rst><ceos><u1><c22><u1><c9>ada clojure "
			"eiffel fortran groovy java kotlin modula ~perl ~python ~rust "
			"~sql<rst><ceos><u1><c23><u1><c9>ada clojure eiffel fortran groovy java "
			"kotlin modula ~perl ~python ~rust ~sql<rst><ceos><u1><c15><u1><c9>ada "
			"clojure eiffel fortran groovy java kotlin ~modula ~perl ~python ~rust "
			"~sql<rst><ceos><u1><c16><u1><c9>ada clojure eiffel fortran groovy java "
			"kotlin ~modula ~perl ~python ~rust ~sql<rst><ceos><u1><c8><u1><c9>ada "
			"clojure eiffel fortran groovy java ~kotlin ~modula ~perl ~python ~rust "
			"~sql<rst><ceos><u1><c9><u1><c9>ada clojure eiffel fortran groovy java "
			"~kotlin ~modula ~perl ~python ~rust ~sql<rst><ceos><u1><c3><u1><c9>ada "
			"clojure eiffel fortran groovy ~java ~kotlin ~modula ~perl ~python ~rust "
			"~sql<rst><ceos><u1><c4><u1><c9>ada clojure eiffel fortran groovy ~java "
			"~kotlin ~modula ~perl ~python ~rust ~sql<rst><ceos><u2><c36><c9>ada clojure "
			"eiffel fortran ~groovy ~java ~kotlin ~modula ~perl ~python ~rust "
			"~sql<rst><ceos><u2><c37><c9>ada clojure eiffel fortran ~groovy ~java ~kotlin "
			"~modula ~perl ~python ~rust ~sql<rst><ceos><u2><c28><c9>ada clojure eiffel "
			"~fortran ~groovy ~java ~kotlin ~modula ~perl ~python ~rust "
			"~sql<rst><ceos><u2><c29><c9>ada clojure eiffel ~fortran ~groovy ~java "
			"~kotlin ~modula ~perl ~python ~rust ~sql<rst><ceos><u2><c21><c9>ada clojure "
			"~eiffel ~fortran ~groovy ~java ~kotlin ~modula ~perl ~python ~rust "
			"~sql<rst><ceos><u2><c22><c9>ada clojure ~eiffel ~fortran ~groovy ~java "
			"~kotlin ~modula ~perl ~python ~rust ~sql<rst><ceos><u2><c13><c9>ada ~clojure "
			"~eiffel ~fortran ~groovy ~java ~kotlin ~modula ~perl ~python ~rust "
			"~sql<rst><ceos><u2><c14><c9>ada ~clojure ~eiffel ~fortran ~groovy ~java "
			"~kotlin ~modula ~perl ~python ~rust ~sql<rst><ceos><u2><c9><c9>~ada ~clojure "
			"~eiffel ~fortran ~groovy ~java ~kotlin ~modula ~perl ~python ~rust "
			"~sql<rst><ceos><u2><c10><c9>~ada ~clojure ~eiffel ~fortran ~groovy ~java "
			"~kotlin ~modula ~perl ~python ~rust ~sql<rst><ceos><u2><c9><c9>~ada ~clojure "
			"~eiffel ~fortran ~groovy ~java ~kotlin ~modula ~perl ~python ~rust "
			"~sql<rst><ceos><c14>\r\n"
			"~ada ~clojure ~eiffel ~fortran ~groovy ~java ~kotlin ~modula ~perl ~python "
			"~rust ~sql\r\n",
			" ".join( _words_[::3] ) + "\n",
			dimensions = ( 10, 40 )
		)
	def test_colors( self_ ):
		self_.check_scenario(
			"<up><cr><c-d>",
			"<c9><black>color_black<rst> <red>color_red<rst> "
			"<green>color_green<rst> <brown>color_brown<rst> <blue>color_blue<rst> "
			"<magenta>color_magenta<rst> <cyan>color_cyan<rst> "
			"<lightgray>color_lightgray<rst> <gray>color_gray<rst> "
			"<brightred>color_brightred<rst> <brightgreen>color_brightgreen<rst> "
			"<yellow>color_yellow<rst> <brightblue>color_brightblue<rst> "
			"<brightmagenta>color_brightmagenta<rst> <brightcyan>color_brightcyan<rst> "
			"<white>color_white<rst><ceos><c70><u2><c9><black>color_black<rst> "
			"<red>color_red<rst> <green>color_green<rst> <brown>color_brown<rst> "
			"<blue>color_blue<rst> <magenta>color_magenta<rst> <cyan>color_cyan<rst> "
			"<lightgray>color_lightgray<rst> <gray>color_gray<rst> "
			"<brightred>color_brightred<rst> <brightgreen>color_brightgreen<rst> "
			"<yellow>color_yellow<rst> <brightblue>color_brightblue<rst> "
			"<brightmagenta>color_brightmagenta<rst> <brightcyan>color_brightcyan<rst> "
			"<white>color_white<rst><ceos><c70>\r\n"
			"color_black color_red color_green color_brown color_blue color_magenta "
			"color_cyan color_lightgray color_gray color_brightred color_brightgreen "
			"color_yellow color_brightblue color_brightmagenta color_brightcyan "
			"color_white\r\n",
			"color_black color_red color_green color_brown color_blue color_magenta color_cyan color_lightgray"
			" color_gray color_brightred color_brightgreen color_yellow color_brightblue color_brightmagenta color_brightcyan color_white\n"
		)
	def test_word_break_characters( self_ ):
		self_.check_scenario(
			"<up><c-left>x<c-left><c-left>x<c-left><c-left>x<c-left><c-left>x<c-left><c-left>x<c-left><c-left>x<cr><c-d>",
			"<c9>one_two three-four five_six "
			"seven-eight<rst><ceos><c48><c9>one_two three-four five_six "
			"seven-eight<rst><ceos><c43><c9>one_two three-four five_six "
			"seven-xeight<rst><ceos><c44><c9>one_two three-four five_six "
			"seven-xeight<rst><ceos><c43><c9>one_two three-four five_six "
			"seven-xeight<rst><ceos><c37><c9>one_two three-four five_six "
			"xseven-xeight<rst><ceos><c38><c9>one_two three-four five_six "
			"xseven-xeight<rst><ceos><c37><c9>one_two three-four five_six "
			"xseven-xeight<rst><ceos><c28><c9>one_two three-four xfive_six "
			"xseven-xeight<rst><ceos><c29><c9>one_two three-four xfive_six "
			"xseven-xeight<rst><ceos><c28><c9>one_two three-four xfive_six "
			"xseven-xeight<rst><ceos><c23><c9>one_two three-xfour xfive_six "
			"xseven-xeight<rst><ceos><c24><c9>one_two three-xfour xfive_six "
			"xseven-xeight<rst><ceos><c23><c9>one_two three-xfour xfive_six "
			"xseven-xeight<rst><ceos><c17><c9>one_two xthree-xfour xfive_six "
			"xseven-xeight<rst><ceos><c18><c9>one_two xthree-xfour xfive_six "
			"xseven-xeight<rst><ceos><c17><c9>one_two xthree-xfour xfive_six "
			"xseven-xeight<rst><ceos><c9><c9>xone_two xthree-xfour xfive_six "
			"xseven-xeight<rst><ceos><c10><c9>xone_two xthree-xfour xfive_six "
			"xseven-xeight<rst><ceos><c54>\r\n"
			"xone_two xthree-xfour xfive_six xseven-xeight\r\n",
			"one_two three-four five_six seven-eight\n",
			command = ReplxxTests._cSample_ + " q1 'w \t-'"
		)
		self_.check_scenario(
			"<up><c-left>x<c-left><c-left>x<c-left><c-left>x<c-left><c-left>x<c-left><c-left>x<c-left><c-left>x<cr><c-d>",
			"<c9>one_two three-four five_six "
			"seven-eight<rst><ceos><c48><c9>one_two three-four five_six "
			"seven-eight<rst><ceos><c37><c9>one_two three-four five_six "
			"xseven-eight<rst><ceos><c38><c9>one_two three-four five_six "
			"xseven-eight<rst><ceos><c37><c9>one_two three-four five_six "
			"xseven-eight<rst><ceos><c33><c9>one_two three-four five_xsix "
			"xseven-eight<rst><ceos><c34><c9>one_two three-four five_xsix "
			"xseven-eight<rst><ceos><c33><c9>one_two three-four five_xsix "
			"xseven-eight<rst><ceos><c28><c9>one_two three-four xfive_xsix "
			"xseven-eight<rst><ceos><c29><c9>one_two three-four xfive_xsix "
			"xseven-eight<rst><ceos><c28><c9>one_two three-four xfive_xsix "
			"xseven-eight<rst><ceos><c17><c9>one_two xthree-four xfive_xsix "
			"xseven-eight<rst><ceos><c18><c9>one_two xthree-four xfive_xsix "
			"xseven-eight<rst><ceos><c17><c9>one_two xthree-four xfive_xsix "
			"xseven-eight<rst><ceos><c13><c9>one_xtwo xthree-four xfive_xsix "
			"xseven-eight<rst><ceos><c14><c9>one_xtwo xthree-four xfive_xsix "
			"xseven-eight<rst><ceos><c13><c9>one_xtwo xthree-four xfive_xsix "
			"xseven-eight<rst><ceos><c9><c9>xone_xtwo xthree-four xfive_xsix "
			"xseven-eight<rst><ceos><c10><c9>xone_xtwo xthree-four xfive_xsix "
			"xseven-eight<rst><ceos><c54>\r\n"
			"xone_xtwo xthree-four xfive_xsix xseven-eight\r\n",
			"one_two three-four five_six seven-eight\n",
			command = ReplxxTests._cSample_ + " q1 'w \t_'"
		)
	def test_no_color( self_ ):
		self_.check_scenario(
			"<up> X<cr><c-d>",
			"<c9>color_black color_red color_green color_brown color_blue "
			"color_magenta color_cyan color_lightgray color_gray color_brightred "
			"color_brightgreen color_yellow color_brightblue color_brightmagenta "
			"color_brightcyan color_white<ceos><c70><u2><c9>color_black color_red "
			"color_green color_brown color_blue color_magenta color_cyan color_lightgray "
			"color_gray color_brightred color_brightgreen color_yellow color_brightblue "
			"color_brightmagenta color_brightcyan color_white "
			"<ceos><c71><u2><c9>color_black color_red color_green color_brown color_blue "
			"color_magenta color_cyan color_lightgray color_gray color_brightred "
			"color_brightgreen color_yellow color_brightblue color_brightmagenta "
			"color_brightcyan color_white X<ceos><c72><u2><c9>color_black color_red "
			"color_green color_brown color_blue color_magenta color_cyan color_lightgray "
			"color_gray color_brightred color_brightgreen color_yellow color_brightblue "
			"color_brightmagenta color_brightcyan color_white X<ceos><c72>\r\n"
			"color_black color_red color_green color_brown color_blue color_magenta "
			"color_cyan color_lightgray color_gray color_brightred color_brightgreen "
			"color_yellow color_brightblue color_brightmagenta color_brightcyan "
			"color_white X\r\n",
			"color_black color_red color_green color_brown color_blue color_magenta color_cyan color_lightgray"
			" color_gray color_brightred color_brightgreen color_yellow color_brightblue color_brightmagenta color_brightcyan color_white\n",
			command = ReplxxTests._cSample_ + " q1 m1"
		)
	def test_backspace_long_line_on_small_term( self_ ):
		self_.check_scenario(
			"<cr><cr><cr><up><backspace><backspace><backspace><backspace><backspace><backspace><backspace><backspace><cr><c-d>",
			"<c9><ceos><c9>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9>\r\n"
			"<brightgreen>replxx<rst>> "
			"<c9>aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa<rst><ceos><c14><u1><c9>aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa<rst><ceos><c13><u1><c9>aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa<rst><ceos><c12><u1><c9>aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa<rst><ceos><c11><u1><c9>aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa<rst><ceos><c10><u1><c9>aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa<rst><ceos><c9><u1><c9>aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa<rst><ceos><c8><u1><c9>aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa<rst><ceos><c7><u1><c9>aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa<rst><ceos><c6><u1><c9>aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa<rst><ceos><c6>\r\n"
			"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n",
			"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n",
			dimensions = ( 10, 40 )
		)
		self_.check_scenario(
			"<cr><cr><cr><up><backspace><backspace><backspace><backspace><backspace><backspace><cr><c-d>",
			"<c9><ceos><c9>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9>\r\n"
			"<brightgreen>replxx<rst>> <c9>a qu ite lo ng li ne of sh ort wo rds wi "
			"ll te st cu rs or mo ve me nt<rst><ceos><c39><u1><c9>a qu ite lo "
			"ng li ne of sh ort wo rds wi ll te st cu rs or mo ve me "
			"n<rst><ceos><c38><u1><c9>a qu ite lo ng li ne of sh ort wo rds wi "
			"ll te st cu rs or mo ve me <rst><ceos><c37><u1><c9>a qu ite lo ng "
			"li ne of sh ort wo rds wi ll te st cu rs or mo ve "
			"me<rst><ceos><c36><u1><c9>a qu ite lo ng li ne of sh ort wo rds "
			"wi ll te st cu rs or mo ve m<rst><ceos><c35><u1><c9>a qu ite lo "
			"ng li ne of sh ort wo rds wi ll te st cu rs or mo ve "
			"<rst><ceos><c34><u1><c9>a qu ite lo ng li ne of sh ort wo rds wi "
			"ll te st cu rs or mo ve<rst><ceos><c33><u1><c9>a qu ite lo ng li "
			"ne of sh ort wo rds wi ll te st cu rs or mo ve<rst><ceos><c33>\r\n"
			"a qu ite lo ng li ne of sh ort wo rds wi ll te st cu rs or mo ve\r\n",
			"a qu ite lo ng li ne of sh ort wo rds wi ll te st cu rs or mo ve me nt\n",
			dimensions = ( 10, 40 )
		)
	def test_reverse_history_search_on_max_match( self_ ):
		self_.check_scenario(
			"<up><c-r><cr><c-d>",
			"<c9>aaaaaaaaaaaaaaaaaaaaa<rst><ceos><c30><c1><ceos><c1><ceos>(reverse-i-search)`': "
			"aaaaaaaaaaaaaaaaaaaaa<c44><c1><ceos><brightgreen>replxx<rst>> "
			"aaaaaaaaaaaaaaaaaaaaa<c30><c9>aaaaaaaaaaaaaaaaaaaaa<rst><ceos><c30>\r\n"
			"aaaaaaaaaaaaaaaaaaaaa\r\n",
			"aaaaaaaaaaaaaaaaaaaaa\n"
		)
	def test_no_terminal( self_ ):
		res = subprocess.run( [ ReplxxTests._cSample_, "q1" ], input = b"replxx FTW!\n", stdout = subprocess.PIPE, stderr = subprocess.PIPE )
		self_.assertSequenceEqual( res.stdout, b"starting...\nreplxx FTW!\n\nExiting Replxx\n" )
	def test_async_print( self_ ):
		self_.check_scenario(
			[ "a", "b", "c", "d", "e", "f<cr><c-d>" ], [
				"<c1><ceos>0\r\n"
				"<brightgreen>replxx<rst>> "
				"<c9><ceos><c9><c9>a<rst><ceos><c10><c9>ab<rst><ceos><c11><c1><ceos>1\r\n"
				"<brightgreen>replxx<rst>> "
				"<c9>ab<rst><ceos><c11><c9>abc<rst><ceos><c12><c9>abcd<rst><ceos><c13><c1><ceos>2\r\n"
				"<brightgreen>replxx<rst>> "
				"<c9>abcd<rst><ceos><c13><c9>abcde<rst><ceos><c14><c9>abcdef<rst><ceos><c15><c9>abcdef<rst><ceos><c15>\r\n"
				"abcdef\r\n",
				"<c1><ceos>0\r\n"
				"<brightgreen>replxx<rst>> "
				"<c9>a<rst><ceos><c10><c9>ab<rst><ceos><c11><c1><ceos>1\r\n"
				"<brightgreen>replxx<rst>> "
				"<c9>ab<rst><ceos><c11><c9>abc<rst><ceos><c12><c9>abcd<rst><ceos><c13><c1><ceos>2\r\n"
				"<brightgreen>replxx<rst>> "
				"<c9>abcd<rst><ceos><c13><c9>abcde<rst><ceos><c14><c9>abcdef<rst><ceos><c15><c9>abcdef<rst><ceos><c15>\r\n"
				"abcdef\r\n",
			],
			command = [ ReplxxTests._cxxSample_, "" ],
			pause = 0.5
		)
		self_.check_scenario(
			[ "<up>", "a", "b", "c", "d", "e", "f<cr><c-d>" ], [
				"<c1><ceos>0\r\n"
				"<brightgreen>replxx<rst>> <c9><ceos><c9><c9>a very long line of "
				"user input, wider then current terminal, the line is wrapped: "
				"<rst><ceos><c11><u2><c9>a very long line of user input, wider then current "
				"terminal, the line is wrapped: a<rst><ceos><c12><u2><c1><ceos>1\r\n"
				"<brightgreen>replxx<rst>> \r\n"
				"\r\n"
				"<u2><c9>a very long line of user input, wider then current terminal, "
				"the line is wrapped: a<rst><ceos><c12><u2><c9>a very long line of user "
				"input, wider then current terminal, the line is wrapped: "
				"ab<rst><ceos><c13><u2><c9>a very long line of user input, wider then current "
				"terminal, the line is wrapped: abc<rst><ceos><c14><u2><c1><ceos>2\r\n"
				"<brightgreen>replxx<rst>> \r\n"
				"\r\n"
				"<u2><c9>a very long line of user input, wider then current terminal, "
				"the line is wrapped: abc<rst><ceos><c14><u2><c9>a very long line of user "
				"input, wider then current terminal, the line is wrapped: "
				"abcd<rst><ceos><c15><u2><c9>a very long line of user input, wider then "
				"current terminal, the line is wrapped: abcde<rst><ceos><c16><u2><c1><ceos>3\r\n"
				"<brightgreen>replxx<rst>> \r\n"
				"\r\n"
				"<u2><c9>a very long line of user input, wider then current terminal, "
				"the line is wrapped: abcde<rst><ceos><c16><u2><c9>a very long line of user "
				"input, wider then current terminal, the line is wrapped: "
				"abcdef<rst><ceos><c17><u2><c9>a very long line of user input, wider then "
				"current terminal, the line is wrapped: abcdef<rst><ceos><c17>\r\n"
				"a very long line of user input, wider then current terminal, the line is "
				"wrapped: abcdef\r\n",
				"<c1><ceos>0\r\n"
				"<brightgreen>replxx<rst>> <c9><ceos><c9><c9>a very long line of user input, "
				"wider then current terminal, the line is wrapped: <rst><ceos><c11><u2><c9>a "
				"very long line of user input, wider then current terminal, the line is "
				"wrapped: a<rst><ceos><c12><u2><c1><ceos>1\r\n"
				"<brightgreen>replxx<rst>> \r\n"
				"\r\n"
				"<u2><c9>a very long line of user input, wider then current terminal, the "
				"line is wrapped: a<rst><ceos><c12><u2><c9>a very long line of user input, "
				"wider then current terminal, the line is wrapped: "
				"ab<rst><ceos><c13><u2><c9>a very long line of user input, wider then current "
				"terminal, the line is wrapped: abc<rst><ceos><c14><u2><c1><ceos>2\r\n"
				"<brightgreen>replxx<rst>> \r\n"
				"\r\n"
				"<u2><c9>a very long line of user input, wider then current terminal, the "
				"line is wrapped: abc<rst><ceos><c14><u2><c9>a very long line of user input, "
				"wider then current terminal, the line is wrapped: "
				"abcd<rst><ceos><c15><u2><c1><ceos>3\r\n"
				"<brightgreen>replxx<rst>> \r\n"
				"\r\n"
				"<u2><c9>a very long line of user input, wider then current terminal, the "
				"line is wrapped: abcd<rst><ceos><c15><u2><c9>a very long line of user input, "
				"wider then current terminal, the line is wrapped: "
				"abcde<rst><ceos><c16><u2><c9>a very long line of user input, wider then "
				"current terminal, the line is wrapped: abcdef<rst><ceos><c17><u2><c9>a very "
				"long line of user input, wider then current terminal, the line is wrapped: "
				"abcdef<rst><ceos><c17>\r\n"
				"a very long line of user input, wider then current terminal, the line is "
				"wrapped: abcdef\r\n",
				"<c1><ceos>0\r\n"
				"<brightgreen>replxx<rst>> <c9>a very long line of user input, wider then "
				"current terminal, the line is wrapped: <rst><ceos><c11><u2><c9>a very long "
				"line of user input, wider then current terminal, the line is wrapped: "
				"a<rst><ceos><c12><u2><c1><ceos>1\r\n"
				"<brightgreen>replxx<rst>> \r\n"
				"\r\n"
				"<u2><c9>a very long line of user input, wider then current terminal, the "
				"line is wrapped: a<rst><ceos><c12><u2><c9>a very long line of user input, "
				"wider then current terminal, the line is wrapped: "
				"ab<rst><ceos><c13><u2><c9>a very long line of user input, wider then current "
				"terminal, the line is wrapped: abc<rst><ceos><c14><u2><c1><ceos>2\r\n"
				"<brightgreen>replxx<rst>> \r\n"
				"\r\n"
				"<u2><c9>a very long line of user input, wider then current terminal, the "
				"line is wrapped: abc<rst><ceos><c14><u2><c9>a very long line of user input, "
				"wider then current terminal, the line is wrapped: "
				"abcd<rst><ceos><c15><u2><c1><ceos>3\r\n"
				"<brightgreen>replxx<rst>> \r\n"
				"\r\n"
				"<u2><c9>a very long line of user input, wider then current terminal, the "
				"line is wrapped: abcd<rst><ceos><c15><u2><c9>a very long line of user input, "
				"wider then current terminal, the line is wrapped: "
				"abcde<rst><ceos><c16><u2><c9>a very long line of user input, wider then "
				"current terminal, the line is wrapped: abcdef<rst><ceos><c17><u2><c9>a very "
				"long line of user input, wider then current terminal, the line is wrapped: "
				"abcdef<rst><ceos><c17>\r\n"
				"a very long line of user input, wider then current terminal, the line is "
				"wrapped: abcdef\r\n"
			],
			"a very long line of user input, wider then current terminal, the line is wrapped: \n",
			command = [ ReplxxTests._cxxSample_, "" ],
			dimensions = ( 10, 40 ),
			pause = 0.5
		)
	def test_async_emulate_key_press( self_ ):
		self_.check_scenario(
			[ "a", "b", "c", "d", "e", "f<cr><c-d>" ], [
				"<c9><yellow>1<rst><ceos><c10><c9><yellow>1<rst>a"
				"<rst><ceos><c11><c9><yellow>1<rst>ab<rst><ceos><c12><c9><yellow>1<rst>ab"
				"<yellow>2<rst><ceos><c13><c9><yellow>1<rst>ab<yellow>2"
				"<rst>c<rst><ceos><c14><c9><yellow>1<rst>ab<yellow>2"
				"<rst>cd<rst><ceos><c15><c9><yellow>1<rst>ab<yellow>2<rst>cd<yellow>3"
				"<rst><ceos><c16><c9><yellow>1<rst>ab<yellow>2<rst>cd<yellow>3<rst>e"
				"<rst><ceos><c17><c9><yellow>1<rst>ab<yellow>2<rst>cd<yellow>3<rst>ef"
				"<rst><ceos><c18><c9><yellow>1<rst>ab<yellow>2<rst>cd<yellow>3<rst>ef<rst><ceos><c18>\r\n"
				"1ab2cd3ef\r\n",
				"<c9><yellow>1<rst>a<rst><ceos><c11><c9><yellow>1<rst>ab"
				"<rst><ceos><c12><c9><yellow>1<rst>ab<yellow>2<rst><ceos><c13><c9><yellow>1<rst>ab"
				"<yellow>2<rst>c<rst><ceos><c14><c9><yellow>1<rst>ab<yellow>2<rst>cd<rst><ceos><c15>"
				"<c9><yellow>1<rst>ab<yellow>2<rst>cd<yellow>3<rst><ceos><c16><c9><yellow>1<rst>ab"
				"<yellow>2<rst>cd<yellow>3<rst>e<rst><ceos><c17><c9><yellow>1<rst>ab<yellow>2<rst>cd"
				"<yellow>3<rst>ef<rst><ceos><c18><c9><yellow>1<rst>ab<yellow>2<rst>cd<yellow>3<rst>ef<rst><ceos><c18>\r\n"
				"1ab2cd3ef\r\n"
			],
			command = [ ReplxxTests._cxxSample_, "123456" ],
			pause = 0.5
		)
	def test_special_keys( self_ ):
		self_.check_scenario(
			"<f1><f2><f3><f4><f5><f6><f7><f8><f9><f10><f11><f12>"
			"<s-f1><s-f2><s-f3><s-f4><s-f5><s-f6><s-f7><s-f8><s-f9><s-f10><s-f11><s-f12>"
			"<c-f1><c-f2><c-f3><c-f4><c-f5><c-f6><c-f7><c-f8><c-f9><c-f10><c-f11><c-f12>"
			"<s-tab><s-left><s-right><s-up><s-down>"
			"<s-home><s-end><c-home><c-end><c-pgup><c-pgdown>"
			"<cr><c-d>",
			"<c1><ceos><F1>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><F2>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><F3>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><F4>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><F5>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><F6>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><F7>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><F8>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><F9>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><F10>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><F11>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><F12>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-F1>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-F2>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-F3>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-F4>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-F5>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-F6>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-F7>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-F8>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-F9>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-F10>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-F11>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-F12>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><C-F1>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><C-F2>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><C-F3>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><C-F4>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><C-F5>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><C-F6>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><C-F7>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><C-F8>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><C-F9>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><C-F10>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><C-F11>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><C-F12>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-Tab>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-Left>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-Right>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-Up>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-Down>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-Home>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><S-End>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><C-Home>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><C-End>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><C-PgUp>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c1><ceos><C-PgDn>\r\n"
			"<brightgreen>replxx<rst>> <c9><ceos><c9><c9><ceos><c9>\r\n"
		)
	def test_overwrite_mode( self_ ):
		self_.check_scenario(
			"<up><home><right><right>XYZ<ins>012<ins>345<cr><c-d>",
			"<c9>abcdefgh<rst><ceos><c17><c9>abcdefgh<rst><ceos><c9>"
			"<c9>abcdefgh<rst><ceos><c10><c9>abcdefgh<rst><ceos><c11>"
			"<c9>abXcdefgh<rst><ceos><c12><c9>abXYcdefgh<rst><ceos><c13>"
			"<c9>abXYZcdefgh<rst><ceos><c14><c9>abXYZ<yellow>0<rst>defgh<rst><ceos><c15>"
			"<c9>abXYZ<yellow>01<rst>efgh<rst><ceos><c16><c9>abXYZ<yellow>012<rst>fgh<rst><ceos><c17>"
			"<c9>abXYZ<yellow>0123<rst>fgh<rst><ceos><c18><c9>abXYZ<yellow>01234<rst>fgh<rst><ceos><c19>"
			"<c9>abXYZ<yellow>012345<rst>fgh<rst><ceos><c20><c9>abXYZ<yellow>012345<rst>fgh<rst><ceos><c23>\r\n"
			"abXYZ012345fgh\r\n",
			"abcdefgh\n"
		)
	def test_verbatim_insert( self_ ):
		self_.check_scenario(
			["<c-v>", rapid( "<ins>" ), "<cr><c-d>"],
			"<c9>^[<brightmagenta>[<yellow>2<rst>~<rst><ceos><c14><c9>^[<brightmagenta>[<yellow>2<rst>~<rst><ceos><c14>\r\n"
			"<ins-key>\r\n"
		)
	def test_hint_delay( self_ ):
		self_.check_scenario(
			["han", "<cr><c-d>"],
			"<c9>h<rst><ceos><c10><c9>ha<rst><ceos><c11><c9>han<rst><ceos><c12><c9>han<rst><ceos>\r\n"
			"        <gray>hans<rst>\r\n"
			"        <gray>hansekogge<rst><u2><c12><c9>han<rst><ceos><c12>\r\n"
			"han\r\n",
			command = [ ReplxxTests._cSample_, "q1", "H200" ]
		)
	def test_complete_next( self_ ):
		self_.check_scenario(
			"<up><c-n><c-n><c-p><c-p><c-p><cr><c-d>",
			"<c9>color_<rst><ceos>\r\n"
			"        <gray>color_black<rst>\r\n"
			"        <gray>color_red<rst>\r\n"
			"        "
			"<gray>color_green<rst><u3><c15><c9><black>color_black<rst><ceos><c20>"
			"<c9><red>color_red<rst><ceos><c18><c9><black>color_black<rst><ceos><c20><c9>color_<rst><ceos>\r\n"

			"        <gray>color_black<rst>\r\n"
			"        <gray>color_red<rst>\r\n"
			"        "
			"<gray>color_green<rst><u3><c15><c9><lightgray>color_normal<rst><ceos><c21><c9><lightgray>color_normal<rst><ceos><c21>\r\n"
			"color_normal\r\n",
			"color_\n"
		)
		self_.check_scenario(
			"l<c-n><c-n><c-p><c-p><cr><c-d>",
			"<c9>l<rst><ceos>\r\n"
			"        <gray>lc_ctype<rst>\r\n"
			"        <gray>lc_time<rst>\r\n"
			"        <gray>lc_messages<rst><u3><c10><c9>lc_<rst><ceos>\r\n"
			"        <gray>lc_ctype<rst>\r\n"
			"        <gray>lc_time<rst>\r\n"
			"        "
			"<gray>lc_messages<rst><u3><c12><c9>lc_ctype<rst><ceos><c17><c9>lc_time<rst><ceos><c16><c9>lc_ctype<rst><ceos><c17><c9>lc_<rst><ceos>\r\n"
			"        <gray>lc_ctype<rst>\r\n"
			"        <gray>lc_time<rst>\r\n"
			"        <gray>lc_messages<rst><u3><c12><c9>lc_<rst><ceos><c12>\r\n"
			"lc_\r\n",
			command = [ ReplxxTests._cSample_, "xlc_ctype,lc_time,lc_messages,zoom", "I1", "q1" ]
		)
		self_.check_scenario(
			"l<c-n><c-n><c-p><c-p><cr><c-d>",
			"<c9>l<rst><ceos>\r\n"
			"        <gray>lc_ctype<rst>\r\n"
			"        <gray>lc_time<rst>\r\n"
			"        <gray>lc_messages<rst><u3><c10><c9>lc_<rst><ceos>\r\n"
			"        <gray>lc_ctype<rst>\r\n"
			"        <gray>lc_time<rst>\r\n"
			"        "
			"<gray>lc_messages<rst><u3><c12><c9>lc_ctype<rst><ceos><c17><c9>lc_<rst><ceos>\r\n"
			"        <gray>lc_ctype<rst>\r\n"
			"        <gray>lc_time<rst>\r\n"
			"        "
			"<gray>lc_messages<rst><u3><c12><c9>lc_messages<rst><ceos><c20><c9>lc_messages<rst><ceos><c20>\r\n"
			"lc_messages\r\n",
			command = [ ReplxxTests._cSample_, "xlc_ctype,lc_time,lc_messages,zoom", "I0", "q1" ]
		)
	def test_disabled_handlers( self_ ):
		self_.check_scenario(
			"<up><left><backspace>4<cr><c-d>",
			"<c9>(+ 1 2)<rst><ceos><c16><c9><brightred>(<rst>+ 1 "
			"2)<rst><ceos><c15><c9><brightred>(<rst>+ 1 "
			")<rst><ceos><c14><c9><brightred>(<rst>+ 1 "
			"4)<rst><ceos><c15><c9><brightred>(<rst>+ 1 4)<rst><ceos><c16>\r\n"
			"thanks for the input: (+ 1 4)\r\n",
			"(+ 1 2)\r\n",
			command = [ ReplxxTests._cSample_, "N", "S" ]
		)
	def test_state_manipulation( self_ ):
		self_.check_scenario(
			"<up><f2>~<cr><c-d>",
			"<c9>replxx<rst><ceos><c15><c9>REPLXX<rst><ceos><c12><c9>REP~LXX<rst><ceos><c13><c9>REP~LXX<rst><ceos><c16>\r\n"
			"REP~LXX\r\n",
			"replxx\n",
			command = [ ReplxxTests._cSample_, "q1" ]
		)
	def test_modify_callback( self_ ):
		self_.check_scenario(
			"<up><home><right><right>*<cr><c-d>",
			"<c9>abcd<brightmagenta>12<rst><ceos><c15><c9>abcd<brightmagenta>12<rst><ceos><c9>"
			"<c9>abcd<brightmagenta>12<rst><ceos><c10><c9>abcd<brightmagenta>12<rst><ceos><c11>"
			"<c9>ababcd<brightmagenta>12<rst>cd<brightmagenta>12<rst><ceos><c15>"
			"<c9>ababcd<brightmagenta>12<rst>cd<brightmagenta>12<rst><ceos><c21>\r\n"
			"ababcd12cd12\r\n",
			"abcd12\n",
			command = [ ReplxxTests._cSample_, "q1", "M1" ]
		)
	def test_paste( self_ ):
		self_.check_scenario(
			rapid( "abcdef<cr><c-d>" ),
			"<c9>a<rst><ceos><c10><c9>abcdef<rst><ceos><c15>\r\nabcdef\r\n"
		)
	def test_history_merge( self_ ):
		with open( "replxx_history_alt.txt", "w" ) as f:
			f.write(
				"### 0000-00-00 00:00:00.001\n"
				"one\n"
				"### 0000-00-00 00:00:00.003\n"
				"three\n"
				"### 0000-00-00 00:00:00.005\n"
				"other\n"
				"### 0000-00-00 00:00:00.009\n"
				"same\n"
				"### 0000-00-00 00:00:00.017\n"
				"seven\n"
			)
			f.close()
		self_.check_scenario(
			"<up><cr><c-d>",
			"<c9><brightmagenta>.<rst>merge<rst><ceos><c15><c9><brightmagenta>.<rst>merge<rst><ceos><c15>\r\n",
			"### 0000-00-00 00:00:00.002\n"
			"two\n"
			"### 0000-00-00 00:00:00.004\n"
			"four\n"
			"### 0000-00-00 00:00:00.006\n"
			"same\n"
			"### 0000-00-00 00:00:00.008\n"
			"other\n"
			"### 0000-00-00 00:00:00.018\n"
			".merge\n"
		)
		with open( "replxx_history_alt.txt", "r" ) as f:
			data = f.read()
			expected = (
				"### 0000-00-00 00:00:00.001\n"
				"one\n"
				"### 0000-00-00 00:00:00.002\n"
				"two\n"
				"### 0000-00-00 00:00:00.003\n"
				"three\n"
				"### 0000-00-00 00:00:00.004\n"
				"four\n"
				"### 0000-00-00 00:00:00.008\n"
				"other\n"
				"### 0000-00-00 00:00:00.009\n"
				"same\n"
				"### 0000-00-00 00:00:00.017\n"
				"seven\n"
				"### "
			)
			self_.assertSequenceEqual( data[:-31], expected )
			self_.assertSequenceEqual( data[-7:], ".merge\n" )
	def test_history_save( self_ ):
		with open( "replxx_history_alt.txt", "w" ) as f:
			f.write(
				"### 0000-00-00 00:00:00.001\n"
				"one\n"
				"### 0000-00-00 00:00:00.003\n"
				"three\n"
				"### 3000-00-00 00:00:00.005\n"
				"other\n"
				"### 3000-00-00 00:00:00.009\n"
				"same\n"
				"### 3000-00-00 00:00:00.017\n"
				"seven\n"
			)
			f.close()
		self_.check_scenario(
			"zoom<cr>.save<cr><up><cr><c-d>",
			"<c9>z<rst><ceos><c10><c9>zo<rst><ceos><c11><c9>zoo<rst><ceos><c12><c9>zoom<rst><ceos><c13><c9>zoom<rst><ceos><c13>\r\n"
			"zoom\r\n"
			"<brightgreen>replxx<rst>> "
			"<c9><brightmagenta>.<rst><ceos><c10><c9><brightmagenta>.<rst>s<rst><ceos><c11><c9><brightmagenta>.<rst>sa<rst><ceos><c12><c9><brightmagenta>.<rst>sav<rst><ceos><c13><c9><brightmagenta>.<rst>save<rst><ceos><c14><c9><brightmagenta>.<rst>save<rst><ceos><c14>\r\n"
			"<brightgreen>replxx<rst>> "
			"<c9>zoom<rst><ceos><c13><c9>zoom<rst><ceos><c13>\r\n"
			"zoom\r\n"
		)
	def test_bracketed_paste( self_ ):
		self_.check_scenario(
			"a0<paste-pfx>b1c2d3e<paste-sfx>4f<cr><c-d>",
			"<c9>a<rst><ceos><c10>"
			"<c9>a<brightmagenta>0<rst><ceos><c11>"
			"<c9>a<brightmagenta>0<rst>b<brightmagenta>1<rst>c<brightmagenta>2<rst>d<brightmagenta>3<rst>e<rst><ceos><c18>"
			"<c9>a<brightmagenta>0<rst>b<brightmagenta>1<rst>c<brightmagenta>2<rst>d<brightmagenta>3<rst>e<brightmagenta>4<rst><ceos><c19>"
			"<c9>a<brightmagenta>0<rst>b<brightmagenta>1<rst>c<brightmagenta>2<rst>d<brightmagenta>3<rst>e<brightmagenta>4<rst>f<rst><ceos><c20>"
			"<c9>a<brightmagenta>0<rst>b<brightmagenta>1<rst>c<brightmagenta>2<rst>d<brightmagenta>3<rst>e<brightmagenta>4<rst>f<rst><ceos><c20>\r\n"
			"a0b1c2d3e4f\r\n",
			command = [ ReplxxTests._cSample_, "q1" ]
		)
		self_.check_scenario(
			"a0<paste-pfx>b1c2d3e<paste-sfx>4f<cr><c-d>",
			"<c9>a<rst><ceos><c10>"
			"<c9>a<brightmagenta>0<rst><ceos><c11>"
			"<c9>a<brightmagenta>0<rst>b<brightmagenta>1<rst>c<brightmagenta>2<rst>d<brightmagenta>3<rst>e<rst><ceos><c18>"
			"<c9>a<brightmagenta>0<rst>b<brightmagenta>1<rst>c<brightmagenta>2<rst>d<brightmagenta>3<rst>e<brightmagenta>4<rst><ceos><c19>"
			"<c9>a<brightmagenta>0<rst>b<brightmagenta>1<rst>c<brightmagenta>2<rst>d<brightmagenta>3<rst>e<brightmagenta>4<rst>f<rst><ceos><c20>"
			"<c9>a<brightmagenta>0<rst>b<brightmagenta>1<rst>c<brightmagenta>2<rst>d<brightmagenta>3<rst>e<brightmagenta>4<rst>f<rst><ceos><c20>\r\n"
			"a0b1c2d3e4f\r\n",
			command = [ ReplxxTests._cSample_, "q1", "B" ]
		)
		self_.check_scenario(
			"a0<left><paste-pfx>/eb<paste-sfx><cr><paste-pfx>/db<paste-sfx><cr><paste-pfx>x<paste-sfx><cr><c-d>",
			"<c9>a<rst><ceos><c10><c9>a<brightmagenta>0<rst><ceos><c11><c9>a<brightmagenta>0<rst><ceos><c10>"
			"<c9>a/eb<brightmagenta>0<rst><ceos><c13><c9>a/eb<brightmagenta>0<rst><ceos><c14>\r\n"
			"a/eb0\r\n"
			"<brightgreen>replxx<rst>> <c9>/db<rst><ceos><c12><c9>/db<rst><ceos><c12>\r\n"
			"/db\r\n"
			"<brightgreen>replxx<rst>> <c9>x<rst><ceos><c10><c9>x<rst><ceos><c10>\r\n"
			"x\r\n",
			command = [ ReplxxTests._cSample_, "q1" ]
		)

def parseArgs( self, func, argv ):
	global verbosity
	res = func( self, argv )
	verbosity = self.verbosity
	return res

if __name__ == "__main__":
	pa = unittest.TestProgram.parseArgs
	unittest.TestProgram.parseArgs = lambda self, argv: parseArgs( self, pa, argv )
	unittest.main()

