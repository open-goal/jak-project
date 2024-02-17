@echo off
cd ..\..
out\build\Release\bin\offline-test -d --iso_data_path iso_data\jak3\ --game jak3
python3 scripts\update_decomp_reference.py failures\ test\decompiler\reference\ --game jak3
RMDIR /Q/S failures
pause