@echo off
C:\Base\Compiler\Dev-Cpp\bin\gcc.exe %1 %2 %3 rep.cpp -orep.exe
::C:\Base\Compiler\Dev-Cpp\bin\gcc.exe -O2 -funroll-loops -march=i486 -mtune=pentiumpro -fomit-frame-pointer -fstrict-aliasing -ffast-math -fforce-addr %1 %2 %3 rep.cpp -orep.exe

set include=C:\Base\COMPILER\VC\include
set lib=C:\Base\COMPILER\VC\lib
::cl -O2 -Gr rep.cpp

set include=C:\Base\Compiler\WATCOM11\h;C:\Base\Compiler\WATCOM11\h\NT
set lib=C:\Base\Compiler\WATCOM11\lib
set watcom=C:\Base\Compiler\WATCOM11
set path=C:\Base\Compiler\WATCOM11\BINNT;%path%
::wcl386 -mf -5r -oneatxh -oi+ rep.cpp
