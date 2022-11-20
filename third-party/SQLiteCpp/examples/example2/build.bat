@REM Copyright (c) 2012-2020 Sebastien Rombauts (sebastien.rombauts@gmail.com)
@REM
@REM Distributed under the MIT License (MIT) (See accompanying file LICENSE.txt
@REM or copy at http://opensource.org/licenses/MIT)
mkdir build
cd build

@REM Generate a Visual Studio solution for latest version found
cmake ..
@if ERRORLEVEL 1 goto onError

@REM Build default configuration (ie 'Debug')
cmake --build .
@if ERRORLEVEL 1 goto onError

goto onSuccess

:onError
@echo An error occured!
:onSuccess
cd ..
