# GNUFortran compiled MAD8 on Linux (Fedora 33)
## Remember that MAD8 can only be compiled in 32 bit due to use of astuce
## Special thanks to Frank Schmidt of CERN for ressurecting this build

# Instructions:

> MAD8 was last built in 2006 (Fedora version 14) and as such has many legacy dependencies which may be difficult to locate. 
> Complete instructions that have worked on Fedora 33 are provided here.
> ## General procedure:
> - install dependencies
> - install suitable GFortran compiler (MAD8 uses Fortran77)
> - make sure to compile in 32 bit (due to astuce) and using the appropriate compiler flags (in Makefile FCOPTS=-O2 -m32 -std=legacy)
> - compile astuce
> - compile mad8

# Dependencies:

> The CERNlib RPM package and other useful packages are provided by Frank Schmidt and stored here: https://github.com/HaroonRafique/cernlib under Useful_Packages
> The following are required (use for example `RPM -U package-name'):
> - for libXm.so.2; lesstif-clients-0.95.2-8.el7.i686.rpm or lesstif-0.95.2-9.fc23.i686.rpm
> - for libpacklib, libkernlib, libmathlib; cernlib-2006-35.fc14.i686.rpm

> Before installing these RPMs other packages may be required
> use `dnf whatprovides library_or_package_name' to find specific dependencies and the packages that contain them
> For some files you may have to scour the web for the old RPM, hopefully everything difficult to locate is provided

> ## Here are the (probably) required dependencies currently supported by Fedora 33 using DNF:
> - libxcrypt-compat-4.4.20-2.fc33.i686
> - libXaw-1.0.13-15.fc33.i686
> - xbae-4.60.4-31.fc33.i686
> - motif
> - motif-devel
> - lapack-3.9.0-5.fc33.i686
> - libgfortran.i686
> - glibc-devel.i686
> - libnsl.i686
> - libX11.i686
> - libXdmcp.i686

# Build Instructions:
 
- `make clean' clean any existing build
- `make astuce' this should compile the astuce executable using the included astuce.f
- `make mad.standard' this should compile the mad.standard executable and move it to mad8s (filename change)

# Run instructions:

> Option 1: call mad8 binary from build directory:
> - `ln -s (full path to mad8.dict) dict' to create a soft link to the mad dictionary file in the local directory
> - e.g. `ln -s /home/HR/Repositories/mad8/GFortran_Fedora_33_Complete/mad8/mad8.dict dict'
> - `/home/HR/Repositories/mad8/GFortran_Fedora_33_Complete/mad8/mad8s < input_filename' to run your input file

> Option 2: copy mad8 binary to run directory:
> - `cp /home/HR/Repositories/mad8/GFortran_Fedora_33_Complete/mad8/mad8s ./mad8s
> - `ln -s (full path to mad8.dict) dict' to create a soft link to the mad dictionary file in the local directory
> - e.g. `ln -s /home/HR/Repositories/mad8/GFortran_Fedora_33_Complete/mad8/mad8.dict dict'
> - `mad8s < input_filename' to run your input file


# Known Issues:
After successful compilation, the following error is given when there is a compiler issue:
```
[HR@localhost MAD8_Test]$ /home/HR/Repositories/mad8/GFort_Attempts/First/mad8/mad8s < ISIS_II_EHRCS.mad 
At line 55 of file /builddir/build/BUILD/cernlib-2006/2006/src/packlib/zebra/mq/mzebra.F
Internal Error: get_unit(): Bad internal unit KIND

Error termination. Backtrace:
#0  0xf7ac7835 in ???
#1  0xf7ac83c2 in ???
#2  0xf7ac897f in ???
#3  0xf7cd62a8 in ???
#4  0xf7cd3c42 in ???
#5  0x8142fbc in ???
#6  0x81241fe in ???
#7  0x804a72d in ???
#8  0xf77af0ad in ???
#9  0x804a775 in ???
#10  0xffffffff in ???
'''
