rem *****************************************
rem *        Delphi CleanUp Batch.          *
rem *                                       *
rem * Clean identcache,local,dcu,exe,       *
rem * map,drc files.                        *
rem * Clean hidden __history folder.        *
rem *                                       *
rem *        Author: Mahdi Safsafi          *
rem *****************************************

@echo off
Setlocal EnableDelayedExpansion

Del "*.identcache" /s/q
Del "*.local" /s/q
Del "*.dcu" /s/q
Del "*.exe" /s/q
Del "*.drc" /s/q
Del "*.map" /s/q

set mustdel=false
For /f %%f in ('dir /s/b/l/a:d') do (
    set "mustdel=false"
	if %%~nf==Win32 (
		if exist "%%f\Debug\" set "mustdel=true"
		if exist "%%f\Release\" set "mustdel=true"
) else if %%~nf==Win64 (
		if exist "%%f\Debug\" set "mustdel=true"
		if exist "%%f\Release\" set "mustdel=true" 
		)
if %%~nf==__history set "mustdel=true"
if !mustdel!==true rd /s/q %%f
)
pause