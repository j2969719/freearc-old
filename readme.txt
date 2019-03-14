It should be easy to compile FreeArc:

== ON UNIX (i've tested it yet only on Fedora7) =========================================

1. Install GHC 6.6.1 (http://haskell.org/ghc/download_ghc_661.html), GCC and make
2. Run in the directory where you've extracted FreeArc sources:
      chmod +x compile*
      ./compile-O2
3. When compilation finishes, you will find "arc" executable in Tests subdirectory


== ON WINDOWS ===========================================================================

1. Download GHC 6.6.1 bundled with C++ compiler (32 mb):
     http://haskell.org/ghc/dist/6.6.1/ghc-6.6.1-i386-windows.exe
2. Install it into C:\Base\Compiler\ghc directory
3. Make sure that make.exe is available via your PATH
4. Run compile-O2.cmd - when it finishes, you will find Arc.exe in Tests subdirectory


== Where to download make.exe ===========================================================

http://sourceforge.net/project/showfiles.php?group_id=2435
make.exe (rename mingw32-make.exe to make.exe)
GNU Make
Current Release: mingw32-make-3.81-2 (2007-03-22 08:44)
mingw32-make-3.81-2.tar.gz  95kb

