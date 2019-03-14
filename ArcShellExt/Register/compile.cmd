@echo off
windres.exe -I. Register.rc res.o
g++.exe -D_UNICODE -DUNICODE -Os -s -mwindows %1 %2 %3 Register.cpp res.o "-oManager of FreeArc integration settings.exe" -lole32
