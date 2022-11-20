@REM Copyright (c) 2012-2022 Sebastien Rombauts (sebastien.rombauts@gmail.com)
@REM
@REM Distributed under the MIT License (MIT) (See accompanying file LICENSE.txt
@REM or copy at http://opensource.org/licenses/MIT)
mkdir build
cd build

@REM Generate a Visual Studio solution for latest version found
REM -DPYTHON_EXECUTABLE=D:\workspace\Corvus\UnrealEngine\Engine\Binaries\ThirdParty\Python\Win64\python.exe
cmake -DSQLITECPP_BUILD_EXAMPLES=ON -DSQLITECPP_BUILD_TESTS=ON -DSQLITECPP_RUN_CPPLINT=OFF ..
@if ERRORLEVEL 1 goto onError

@REM Build default configuration (ie 'Debug')
cmake --build .
@if ERRORLEVEL 1 goto onError

@REM Build and run tests
ctest --output-on-failure
@if ERRORLEVEL 1 goto onError

goto onSuccess

:onError
@echo An error occured!
:onSuccess
cd ..
