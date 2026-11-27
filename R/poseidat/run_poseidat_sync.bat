@echo off
REM ── Poseidat daily sync ──────────────────────────────────────────────────────

SET RSCRIPT="C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
SET SCRIPT="C:\GIT\digivloot\R\poseidat\Poseidat_sync_daily.R"
SET LOGFILE="C:\Users\MartinPastoors\Martin Pastoors\DIGIvloot - data - data\logs\poseidat_sync_%DATE:~-4,4%%DATE:~-7,2%%DATE:~0,2%.log"

if not exist "C:\Users\MartinPastoors\Martin Pastoors\DIGIvloot - data - data\logs" mkdir "C:\Users\MartinPastoors\Martin Pastoors\DIGIvloot - data - data\logs"

REM ── Check network profile — skip if on mobile hotspot ──────────────────────
SET ALLOWED_NETWORK=0
@echo off
REM ── Check network profile — skip if not on trusted network ─────────────────
SET ALLOWED_NETWORK=0

FOR /F "tokens=*" %%N IN ('powershell -Command "Get-NetConnectionProfile | Select-Object -ExpandProperty Name"') DO (
    echo %%N | findstr /I /C:"Fairphone" > nul || SET ALLOWED_NETWORK=1
)

IF "%ALLOWED_NETWORK%"=="0" (
    echo === Skipping sync -- running on hotspot >> %LOGFILE%
    echo === Skipping sync -- running on hotspot
    exit /b 0
)
echo === Network check passed
echo === Network check passed >> %LOGFILE%

echo === Poseidat sync started: %DATE% %TIME% >> %LOGFILE%
%RSCRIPT% %SCRIPT% >> %LOGFILE% 2>&1
echo === Exit code: %ERRORLEVEL% >> %LOGFILE%

REM Show last 30 lines of log on screen
powershell -Command "Get-Content '%LOGFILE%' | Select-Object -Last 30"
pause
