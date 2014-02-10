@echo off

echo Welcome to dummymagma v1.0!

:begin
    set /p input="> "
    REM The next line should be deleted by comint
    echo ^> %input%
    REM The next line should appear
    echo Input: %input%
goto begin

