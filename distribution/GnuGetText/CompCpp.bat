rem Compile Pascal unit for use in CPP Builder

dcc32.exe -jphne -$O+ -$D0 -$L- -$Y- GnuGetText.pas
dcc32.exe -jphne -$O+ -$D0 -$L- -$Y- GnuGetTextInit.pas
dcc32.exe -jphne -$O+ -$D0 -$L- -$Y- GgtDummy.pas

pause

