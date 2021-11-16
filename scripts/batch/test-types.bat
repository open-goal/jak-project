@echo off
cd ..\..
out\build\Release\bin\goalc-test --gtest_filter="TypeConsistency.TypeConsistency"
pause