@echo off
rem Standard-Skript zum Aufruf des Arnold-Assemblers
rem V.Pohlers 2009
rem als Parameter wird der Name der asm-Datei übergeben

set file=%1
if "%1"=="" set file=mon_v31_16-as.asm

for %%i in (%file%) do set file=%%~ni

echo -------- %file% -----------

set bin=%cd:~,2%\hobby3\Programme\as\bin

%bin%\as.exe -L %file%.asm -a
%bin%\p2bin.exe -r $-$ "%file%.p"
%bin%\plist.exe "%file%.p"

%bin%\bdiff %file%.bin %file%.com
