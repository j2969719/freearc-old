@echo off
t C:\Base\Compiler\Dev-Cpp\bin\g++.exe -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -O6 --param inline-unit-growth=999999 -funroll-loops -fno-exceptions -fno-rtti -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion -march=i486 -mtune=pentiumpro -fomit-frame-pointer -fstrict-aliasing -ffast-math -fforce-addr %1 %2 %3 main.cpp -otor.exe
:: -DSTATS -DDEBUG -DFULL_COMPILE
