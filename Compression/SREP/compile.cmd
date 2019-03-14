@echo off
set options=-DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -DUNICODE -D_UNICODE -I../_Encryption/headers srep.cpp ../Common.cpp
set options_ms=-DFREEARC_64BIT -MP -Gy -GL -GR- %options% user32.lib shell32.lib ole32.lib -link -LARGEADDRESSAWARE
set options_ms_cl=-O2 %options_ms%
set options_ms_icl=-w -O3 -Qip %options_ms%

g++ %options% -O3 -osrep.exe -lole32 -s -Xlinker --large-address-aware

call "C:\Program Files (x86)\Microsoft Visual Studio 9.0\VC\bin\vcvars32.bat"
cl  -Fesrep32.exe %options_ms_cl%

call "C:\Program Files (x86)\Microsoft Visual Studio 9.0\VC\bin\amd64\vcvarsamd64.bat"
cl  -Fesrep64.exe %options_ms_cl%

call "C:\Program Files (x86)\Intel\Compiler\11.0\066\cpp\bin\iclvars.bat" ia32
icl -Fesrep32i.exe -arch:IA32 %options_ms_icl%
iccpatch.exe srep32i.exe

call "C:\Program Files (x86)\Intel\Compiler\11.0\066\cpp\bin\iclvars.bat" intel64
icl -Fesrep64i.exe %options_ms_icl%
iccpatch.exe srep64i.exe

del *.exe.bak *.obj