@echo off
cd ..\..
python3 scripts\gsrc\update-gsrc-via-refs.py --game jak2 --decompiler out\build\Release\bin\decompiler.exe --decompiler_config .\decompiler\config\jak2_ntsc_v1.jsonc
pause