@echo off
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0dev.ps1" compile %*
exit /b %ERRORLEVEL%
