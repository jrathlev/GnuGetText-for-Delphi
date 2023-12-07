@rem compile Personal Backup help files

"c:\Program Files (x86)\HTML Help Workshop\hhc.exe" en\translate-e.hhp
"c:\Program Files (x86)\HTML Help Workshop\hhc.exe" de\translate-d.hhp

rem xcopy /Y /D *.chm ..\Debug\Win32\
rem xcopy /Y /D *.chm ..\Release\Win32\

pause

