@echo off
:: The caret ^ acts as a line break but does not make the next line a different command.
out\build\Release\bin\decompiler decompiler\config\jak1_ntsc_black_label.jsonc iso_data ^
decompiler_out
:: The pause command makes the batch operation halt with a "Press any key to continue..." message.
:: Useful for example if the decompiler failed and exited for whatever reason, or for verifying its
:: success.
pause
