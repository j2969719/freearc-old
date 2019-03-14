@echo off
t C:\Base\Compiler\Dev-Cpp\bin\g++.exe -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion %1 %2 %3 main.cpp -otor.exe
::-DSTATS, -DDEBUG
