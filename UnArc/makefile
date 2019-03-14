include ../common.mak

Extractor_DEFINES = -DFREEARC_DECOMPRESS_ONLY -DFREEARC_LARGE_FILES
OBJDIR  = $(TEMPDIR)/Extractor
LINKOBJ = $(OBJDIR)/Environment.o $(OBJDIR)/CompressionLibrary.o $(OBJDIR)/C_LZMA.o
HEADERS =  ../Compression/Compression.h ../Compression/Common.h makefile


ALL: unarc.exe arc.sfx FreeArc.fmt

CODE_FLAGS  = -fno-exceptions -fno-rtti -Wall \
              -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -O2 -march=i486 -mtune=pentiumpro -ffast-math \
              -fomit-frame-pointer -fstrict-aliasing \
              -fforce-addr
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES) $(Extractor_DEFINES)


unarc.exe: $(OBJDIR)/unarc.o $(LINKOBJ)
	$(GCC) -L$(LIBDIR) $< $(LINKOBJ) -lstdc++ -o unarc.exe
	strip unarc.exe

arc.sfx: $(OBJDIR)/ArcSFX.o $(LINKOBJ)
	$(GCC) -L$(LIBDIR) $< $(LINKOBJ) -lstdc++ -o arc.sfx
	strip arc.sfx

FreeArc.fmt: $(OBJDIR)/FarPlugin.o FarPlugin.def $(LINKOBJ)
	$(DLLWRAP) --driver-name c++ $< $(LINKOBJ) -L$(LIBDIR) -def FarPlugin.def -s -o FreeArc.fmt

$(OBJDIR)/ArcSFX.o: ArcSFX.cpp unarc.cpp ArcStructure.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -IC:\Base\Tools\Far\PlugDoc\Headers.c -o $*.o $<

$(OBJDIR)/unarc.o: unarc.cpp ArcStructure.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -IC:\Base\Tools\Far\PlugDoc\Headers.c -o $*.o $<

$(OBJDIR)/FarPlugin.o: FarPlugin.cpp ArcStructure.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -IC:\Base\Tools\Far\PlugDoc\Headers.c -o $*.o $<

$(OBJDIR)/Environment.o: ../Environment.cpp ../Environment.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/CompressionLibrary.o: ../Compression/CompressionLibrary.cpp $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_LZMA.o: ../Compression/LZMA/C_LZMA.cpp ../Compression/LZMA/C_LZMA.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -D_NO_EXCEPTIONS -o $*.o $<
