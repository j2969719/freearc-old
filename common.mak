DEFINES = -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER
TEMPDIR = c:/temp/out/FreeArc
GHCDIR  = C:\Base\Compiler\ghc
LIBDIR  = $(GHCDIR)\gcc-lib
INCDIR  = $(GHCDIR)\include\mingw
GCC     = $(GHCDIR)\gcc.exe -B$(LIBDIR) -I$(INCDIR)
DLLWRAP = $(GHCDIR)\gcc-lib\dllwrap.exe
