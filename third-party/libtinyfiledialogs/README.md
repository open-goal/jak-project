## Official version hosted at https://sourceforge.net/projects/tinyfiledialogs/

This version is _not_ official and is currently updated as required by https://github.com/haxelime/lime only

# tinyfiledialogs

<pre>
 _________
/         \   tiny file dialogs ( cross-platform C C++ )
|tiny file|   v2.9.3 [July 12, 2017] zlib licence
| dialogs |   InputBox PasswordBox MessageBox ColorPicker
\____  ___/   OpenFileDialog SaveFileDialog SelectFolderDialog		
     \|       ASCII UTF-8 (and also MBCS UTF-16 for windows)
              Native dialog library for WINDOWS MAC OSX GTK+ QT CONSOLE
  
SSH supported via automatic switch to console mode or X11 forwarding

tested with C & C++ compilers on 
    Visual Studio MinGW OSX LINUX FREEBSD OPENBSD ILLUMOS SOLARIS MINIX RASPBIAN
using
    Gnome Kde Mate Cinnamon Unity Lxde Lxqt Xfce Enlightenment
    WindowMaker IceWm Cde Jds OpenBox Awesome Jwm

bindings for LUA and C# dll
included in LWJGL(java), rust, Allegrobasic

                   http://tinyfiledialogs.sourceforge.net
                git://git.code.sf.net/p/tinyfiledialogs/code
 _________________________________________________________________________
|                                                                         |
| CONTACT me directly via the email address at the top of the header file |
|_________________________________________________________________________|

if you absolutely want them, the windows only wchar_t prototypes are in the header file

int tinyfd_messageBox (
    char const * const aTitle , // ""
    char const * const aMessage , // "" may contain \n \t
    char const * const aDialogType , // "ok" "okcancel" "yesno" "yesnocancel"
    char const * const aIconType , // "info" "warning" "error" "question"
    int const aDefaultButton ) ;
        // 0 for cancel/no , 1 for ok/yes , 2 for no in yesnocancel

char const * tinyfd_inputBox (
    char const * const aTitle , // ""
    char const * const aMessage , // "" may NOT contain \n \t on windows
    char const * const aDefaultInput ) ; // "" , if NULL it's a passwordBox
        // returns NULL on cancel

char const * tinyfd_saveFileDialog (
    char const * const aTitle , // ""
    char const * const aDefaultPathAndFile , // ""
    int const aNumOfFilterPatterns , // 0
    char const * const * const aFilterPatterns , // NULL | {"*.txt"}
    char const * const aSingleFilterDescription ) ; // NULL | "text files"
        // returns NULL on cancel

char const * tinyfd_openFileDialog (
    char const * const aTitle , // ""
    char const * const aDefaultPathAndFile , // ""
    int const aNumOfFilterPatterns , // 0
    char const * const * const aFilterPatterns , // NULL {"*.jpg","*.png"}
    char const * const aSingleFilterDescription , // NULL | "image files"
    int const aAllowMultipleSelects ) ; // 0
        // in case of multiple files, the separator is |
        // returns NULL on cancel

char const * tinyfd_selectFolderDialog (
    char const * const aTitle , // ""
    char const * const aDefaultPath ) ; // ""
        // returns NULL on cancel

char const * tinyfd_colorChooser(
    char const * const aTitle , // ""
    char const * const aDefaultHexRGB , // NULL or "#FF0000”
    unsigned char const aDefaultRGB[3] , // { 0 , 255 , 255 }
    unsigned char aoResultRGB[3] ) ; // { 0 , 0 , 0 }
        // returns the hexcolor as a string "#FF0000"
        // aoResultRGB also contains the result
        // aDefaultRGB is used only if aDefaultHexRGB is NULL
        // aDefaultRGB and aoResultRGB can be the same array
        // returns NULL on cancel

- This is not for android nor ios.
- The code is pure C, perfectly compatible with C++.
- the windows only wchar_t (utf-16) prototypes are in the header file
- windows is fully supported from XP to 10 (maybe even older versions)
- C# & LUA via dll, see files in the folder EXTRAS 
- OSX supported from 10.4 to 10.12 (maybe even older versions)
- Avoid using " and ' in titles and messages.
- There's one file filter only, it may contain several patterns.
- If no filter description is provided,
  the list of patterns will become the description.
- char const * filterPatterns[3] = { "*.obj" , "*.stl" , "*.dxf" } ;
- On windows link against Comdlg32.lib and Ole32.lib
  (on windows the no linking claim is a lie)
  This linking is not compulsary for console mode (see header file).
- On unix: it tries command line calls, so no such need (NO LINKING).
- On unix you need applescript, zenity, matedialog, qarma, kdialog, Xdialog,
  python2/tkinter or dialog (will open a terminal if running without console).
- One of those is already included on most (if not all) desktops.
- In the absence of those it will use gdialog, gxmessage or whiptail
  with a textinputbox.
- If nothing is found, it switches to basic console input,
  it opens a console if needed (requires xterm + bash).
- Use windows separators on windows and unix separators on unix.
- String memory is preallocated statically for all the returned values.
- File and path names are tested before return, they are valid.
- If you pass only a path instead of path + filename,
  make sure it ends with a separator.
- tinyfd_forceConsole=1; at run time, forces dialogs into console mode.
- On windows, console mode only make sense for console applications.
- Mutiple selects are not allowed in console mode.
- The package dialog must be installed to run in enhanced console mode.
  It is already installed on most unix systems.
- On osx, the package dialog can be installed via http://macports.org
- On windows, for enhanced console mode,
  dialog.exe should be copied somewhere on your executable path.
  It can be found at the bottom of the following page:
  http://andrear.altervista.org/home/cdialog.php
- If dialog is missing, it will switch to basic console input.
- You can query the type of dialog that will be use.
- MinGW needs gcc >= v4.9 otherwise some headers are incomplete.

- Here is the Hello World:
            if a console is missing, it will use graphic dialogs
            if a graphical display is absent, it will use console dialogs


hello.c
#include <stdio.h>
#include <string.h>
#include "tinyfiledialogs.h"
int main()
{
	char const * lTmp;
	char const * lTheSaveFileName;
	char const * lTheOpenFileName;
	char const * lTheSelectFolderName;
	char const * lTheHexColor;
	char const * lWillBeGraphicMode;
	unsigned char lRgbColor[3];
	FILE * lIn;
	char lBuffer[1024];
	char lThePassword[1024];
	char const * lFilterPatterns[2] = { "*.txt", "*.text" };

	lWillBeGraphicMode = tinyfd_inputBox("tinyfd_query", NULL, NULL);

	if (lWillBeGraphicMode)
	{
		strcpy(lBuffer, "graphic mode: ");
	}
	else
	{
		strcpy(lBuffer, "console mode: ");
	}

	strcat(lBuffer, tinyfd_response);
	strcpy(lThePassword, "tinyfiledialogs v");
	strcat(lThePassword, tinyfd_version);
	tinyfd_messageBox(lThePassword, lBuffer, "ok", "info", 0);

	if ( lWillBeGraphicMode && ! tinyfd_forceConsole )
	{
		tinyfd_forceConsole = ! tinyfd_messageBox("Hello World",
			"graphic dialogs [yes] / console mode [no]?",
			"yesno", "question", 1);
	}

	lTmp = tinyfd_inputBox(
		"a password box", "your password will be revealed", NULL);

	if (!lTmp) return 1 ;

	/* copy lTmp because saveDialog would overwrites
	inputBox static buffer in basicinput mode */

	strcpy(lThePassword, lTmp);

	lTheSaveFileName = tinyfd_saveFileDialog(
		"let us save this password",
		"passwordFile.txt",
		2,
		lFilterPatterns,
		NULL);

	if (! lTheSaveFileName)
	{
		tinyfd_messageBox(
			"Error",
			"Save file name is NULL",
			"ok",
			"error",
			1);
		return 1 ;
	}

	lIn = fopen(lTheSaveFileName, "w");
	if (!lIn)
	{
		tinyfd_messageBox(
			"Error",
			"Can not open this file in write mode",
			"ok",
			"error",
			1);
		return 1 ;
	}
	fputs(lThePassword, lIn);
	fclose(lIn);

	lTheOpenFileName = tinyfd_openFileDialog(
		"let us read the password back",
		"",
		2,
		lFilterPatterns,
		NULL,
		0);

	if (! lTheOpenFileName)
	{
		tinyfd_messageBox(
			"Error",
			"Open file name is NULL",
			"ok",
			"error",
			1);
		return 1 ;
	}

	lIn = fopen(lTheOpenFileName, "r");

	if (!lIn)
	{
		tinyfd_messageBox(
			"Error",
			"Can not open this file in read mode",
			"ok",
			"error",
			1);
		return(1);
	}
	lBuffer[0] = '\0';
	fgets(lBuffer, sizeof(lBuffer), lIn);
	fclose(lIn);

	tinyfd_messageBox("your password is",
			lBuffer, "ok", "info", 1);

	lTheSelectFolderName = tinyfd_selectFolderDialog(
		"let us just select a directory", NULL);

	if (!lTheSelectFolderName)
	{
		tinyfd_messageBox(
			"Error",
			"Select folder name is NULL",
			"ok",
			"error",
			1);
		return 1;
	}

	tinyfd_messageBox("The selected folder is",
		lTheSelectFolderName, "ok", "info", 1);

	lTheHexColor = tinyfd_colorChooser(
		"choose a nice color",
		"#FF0077",
		lRgbColor,
		lRgbColor);

	if (!lTheHexColor)
	{
		tinyfd_messageBox(
			"Error",
			"hexcolor is NULL",
			"ok",
			"error",
			1);
		return 1;
	}

	tinyfd_messageBox("The selected hexcolor is",
		lTheHexColor, "ok", "info", 1);

	return 0;
}


OSX :
$ gcc -o hello.app hello.c tinyfiledialogs.c

UNIX :
$ gcc -o hello hello.c tinyfiledialogs.c
( or clang tcc cc CC )

MinGW (needs gcc >= v4.9 otherwise some headers are incomplete):
> gcc -o hello.exe hello.c tinyfiledialogs.c -LC:/mingw/lib -lcomdlg32 -lole32
(unfortunately some headers are missing with tcc)

VisualStudio :
    Create a console application project,
    it links against Comdlg32.lib & Ole32.lib.

</pre>
