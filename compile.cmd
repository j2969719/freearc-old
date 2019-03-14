@set exe=Tests\Arc.exe
@set ctempdir=C:\temp\out\FreeArc
@set tempdir=%ctempdir%%1
@md  %tempdir% >nul 2>nul
@md  %ctempdir% >nul 2>nul
@set defines=-DFREEARC_PACKED_STRINGS -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -optc-DFREEARC_WIN -optc-DFREEARC_INTEL_BYTE_ORDER
@rem ******** -DFREEARC_UNIX -DFREEARC_MOTOROLA_BYTE_ORDER -DFREEARC_ONLY_ALIGNED_ACCESS *******
@cd Compression
@call compile
@cd ..
@make
@del %exe% >nul 2>nul
@set c_modules=%ctempdir%/Environment.o %ctempdir%/URL.o %ctempdir%/CompressionLibrary.o %ctempdir%/C_PPMD_Parser.o %ctempdir%/C_PPMD_Compress.o %ctempdir%/C_PPMD_Decompress.o %ctempdir%/C_LZP.o %ctempdir%/C_LZMA.o %ctempdir%/C_BCJ.o %ctempdir%/C_GRZip.o %ctempdir%/C_Dict.o %ctempdir%/C_REP.o %ctempdir%/C_MM.o %ctempdir%/C_TTA.o %ctempdir%/C_Tornado.o %ctempdir%/C_Delta.o %ctempdir%/C_External.o %ctempdir%/C_Encryption.o -lstdc++ -lwininet C:\Base\Compiler\ghc\gcc-lib\CRT_noglob.o -optl -s -optl --large-address-aware
@set options=-iCompression -threaded -cpp -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances -fallow-incoherent-instances -fno-monomorphism-restriction -fbang-patterns
@set ghc_rts_options=+RTS -A10m -H50m -M300m
ghc.exe --make %1 Arc.hs %options% %defines% %c_modules% -odir %tempdir% -hidir %tempdir% -o %exe% %ghc_rts_options%
@del Compression\CompressionLib_stub.? >nul 2>nul
