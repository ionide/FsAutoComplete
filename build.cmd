@echo off

dotnet tool restore
.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

dotnet fake run %*
