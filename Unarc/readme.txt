SFX modules and tiny decompressor for FreeArc (http://freearc.org)

SFX modules, classification by OS/subsystem:
   arc*.sfx.linux - console Linux-i386 self-extractors
   freearc*.sfx   - GUI Win32 self-extractors
   arc*.sfx       - console Win32 self-extractors

SFX modules, classification by included compression methods:
    *arc.sfx      - full, includes decomression code for all the 11 FreeArc algorithms
    *arc-mini.sfx - without mm/tta/grzip/tornado. for archives created with options like -m5 -mm-
    *arc-tiny.sfx - also without rep/lzp/ppmd. for archives created with options like -m5x -mm-

Modules for producing simple installers:
    freearc-installer.sfx - decompresses files into temprary dirrectory, runs setup.exe, deletes extracted files when it finished
    freearc-installer-nodelete.sfx - decompresses files into temprary dirrectory and runs setup.exe

DLL for decompression FreeArc archives in 3rd-party programs:
    unarc.dll - provides FreeArcExtract() function, example of its use in freearc_example.iss

Standalone extractors of FreeArc archives:
    unarc.exe - console Win32 extractor
    unarc     - console Linux-i386 extractor
