@echo off
cd ..\..
out\build\Release\bin\offline-test -d --iso_data_path iso_data\jak2\ --game jak2
scripts\update_decomp_reference.py failures\ test\decompiler\reference\
RMDIR /Q/S failures
pause