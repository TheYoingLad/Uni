
@ECHO OFF
if not exist junit5all.jar (
    echo Missing file: junit5all.jar
    goto END
)

if "%~2"=="" (
    echo Usage: check.bat ^<path of tester file^> ^<tester class^> [CheckThat options] [JUnit properties]
    echo Useful CheckThat options:
    echo    -Dlang=HU or -Dlang=EN  For structural tester error messages.
    echo    -Dsummary=time/full     Use either time or full to see more JUnit summary details.
    echo    -Derrors=verbose        If you really, REALLY want to see the full stack trace.
    echo Useful JUnit properties:
    echo    --disable-ansi-colors   For monochrome terminals.
    echo[
    echo Note: in PowerShell terminals, options have to be surrounded using this very silly syntax:
    echo    '"-Dlang=EN"'
    echo[
    echo Note: passing more than one option can be tricky.
    echo Suggested: see the executed java and javac commands and manually modify them.
    goto END
)

set TARGET1="%~1"
set TARGET2="%~2"
if not "%~3"=="" (
    set PROPS="%~3"
)
if not "%~4"=="" (
    set OPTS="%~4"
)
@ECHO ON


javac -cp ".;junit5all.jar;checkthat.jar" %TARGET1%

: Stops the batch file if there was a compilation error
@if %ERRORLEVEL% NEQ 0 exit /b %ERRORLEVEL%

java %PROPS% -javaagent:checkagent.jar -jar junit5all.jar execute %OPTS% -E junit-vintage --disable-banner -cp . -cp checkthat.jar -c %TARGET2%


:end
