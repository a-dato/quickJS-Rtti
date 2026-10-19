@echo off
rem Optional: copies the runtime DLLs from redist\ next to the exe.
rem Not required for running from the project: the program preloads them from redist\.
setlocal
set OUT=%~1
if "%OUT%"=="" set OUT=%~dp0Win64\Debug
copy /y "%~dp0redist\quickjs64.dll" "%OUT%\" >nul
copy /y "%~dp0redist\WebView2Loader.dll" "%OUT%\" >nul
exit /b 0
