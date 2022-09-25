@echo off
cd ..\..
out\build\Release\bin\offline-test -d --iso_data_path iso_data\jak1\ --game jak1
python3 scripts\update_decomp_reference.py failures\ test\decompiler\reference\ --game jak1
RMDIR /Q/S failures
pause