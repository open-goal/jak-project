@echo off
cd ..\..
out\build\Release\bin\goalc-test --gtest_filter="Jak3TypeConsistency.TypeConsistency*"
pause