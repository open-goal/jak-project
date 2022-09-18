@echo off
cd ..\..
out\build\Release\bin\offline-test -d --iso_data_path iso_data\jak1\ --game jak1
scripts\update_decomp_reference.py failures\ test\decompiler\reference\jak1\
RMDIR /Q/S failures
pause