rem *****************************************
rem *        Delphi CleanUp Batch.          *
rem *                                       *
rem * Clean identcache,local,dcu,exe files. *
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

rem Delete hidden sub folder.
For /f %%f in ('dir /s/b/l/a:dh') do (
if %%~nf==__history rd /s/q %%f
)

