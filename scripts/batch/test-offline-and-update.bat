@echo off
cd ..\..
out\build\Release\bin\offline-test --dump-mode iso_data\jak1\
scripts\update_decomp_reference.py failures\ test\decompiler\reference\
RMDIR /Q/S failures
pause