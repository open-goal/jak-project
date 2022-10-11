@echo off
cd ..\..
out\build\Release\bin\offline-test -d --iso_data_path iso_data\jak2\ --game jak2
python3 scripts\update_decomp_reference.py failures\ test\decompiler\reference\ --game jak2
RMDIR /Q/S failures
pause