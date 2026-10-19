@echo off
rem Command line build (Win64 Debug). The IDE build uses QuickJSWebViewSyncTest.dproj.
setlocal
set BDS=C:\Program Files (x86)\Embarcadero\Studio\37.0
set BRANCH=master
set OUT=%~dp0Win64\Debug
if not exist "%OUT%\dcu" mkdir "%OUT%\dcu"
set SRC=D:\adato_quickJS\Source;D:\adato_lynx_%BRANCH%\External\ScriptGate;D:\adato_lynx_%BRANCH%\External;D:\adato_lynx_%BRANCH%\LynxBot
set SRC=%SRC%;D:\fmx-datacontrol\dn4d\Source;D:\fmx-datacontrol\datamodel\General;D:\fmx-datacontrol\datamodel\DataModel
set SRC=%SRC%;D:\fmx-datacontrol\objectModel;D:\fmx-datacontrol\controls
set SRC=%SRC%;D:\fmx-datacontrol\datacontrol\Scrollable;D:\fmx-datacontrol\datacontrol\Tree;D:\fmx-datacontrol\datacontrol\Package
set SRC=%SRC%;D:\fmx-datacontrol\grid_vcl\Css;D:\fmx-datacontrol\grid_vcl\General;D:\fmx-datacontrol\grid_vcl\Tree
set SRC=%SRC%;D:\fmx-datacontrol\datamodel\Package
set LIB=%BDS%\lib\Win64\release;%BDS%\lib\Win64\release\en
"%BDS%\bin\dcc64.exe" -B -Q -$D+ -$L+ -V -DDEBUG ^
  -NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win ^
  -U"%LIB%;%SRC%" -I"%SRC%" -R"%LIB%;%SRC%" ^
  -E"%OUT%" -NU"%OUT%\dcu" ^
  "%~dp0QuickJSWebViewSyncTest.dpr"
if errorlevel 1 exit /b 1
rem DLLs are preloaded from redist at startup
