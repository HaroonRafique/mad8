# GNUFortran compiled MAD8 on Linux (Fedora 33)
## Remember that MAD8 can only be compiled in 32 bit due to use of astuce

# Instructions:
## If you cannot install the provided CERNlib RPMs because of dependencies use `dnf whatprovides library_or_package_name'
## Some files you may have to scour the web for the old RPM
## Here are the ones currently supported by Fedora 33 using DNF:
- libxcrypt-compat-4.4.20-2.fc33.i686
- libXaw-1.0.13-15.fc33.i686
- xbae-4.60.4-31.fc33.i686
- motif
- motif-devel
- lapack-3.9.0-5.fc33.i686

# Other dependencies are currently stored in this repository:
https://github.com/HaroonRafique/cernlib under Useful_Packages

# Installation: 
- `make clean' clean any existing build
- `make astuce' this should compile the astuce executable using the included astuce.f
- `make mad.standard' this should compile the mad.standard executable and move it to mad8s (filename change)
- `ln -s ma8.dict dict' to create a soft link to the mad dictionary file
- `mad8s < input_filename' to run your input file


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
