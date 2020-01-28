@echo off

dotnet tool restore
dotnet restore
if errorlevel 1 (
  exit /b %errorlevel%
)

dotnet fake run %*
