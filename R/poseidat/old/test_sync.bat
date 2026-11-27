@echo off
REM ── Check network profile — skip if not on trusted network ─────────────────
SET ALLOWED_NETWORK=0

FOR /F "tokens=*" %%N IN ('powershell -Command "Get-NetConnectionProfile | Select-Object -ExpandProperty Name"') DO (
    echo %%N | findstr /I /C:"Fairphone" > nul || SET ALLOWED_NETWORK=1
)

IF "%ALLOWED_NETWORK%"=="0" (
    echo === Skipping sync -- running on hotspot
    exit /b 0
)
echo === Network check passed