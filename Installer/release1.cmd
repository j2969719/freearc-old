::Compile all components
cd ..\Unarc
make
upx --lzma -9 *.sfx
::upx -d *.sfx
::upx --ultra-brute *.sfx
cd ..

cmd /c compile-O2.cmd
cmd /c compile-GUI-O2.cmd

cd Compression
del facompress*.dll
cmd /c compile-dll.cmd

cd ..\ArcShellExt
cmd /c compile-32.cmd
cmd /c compile-64.cmd

cd Register
cmd /c compile.cmd
cmd /c compile64.cmd

cd ..\..\Installer
