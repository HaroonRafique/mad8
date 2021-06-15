# Installation instructions using GCC-GFortran on Fedora 33 64 bit
## Install the following 32 bit libraries (MAD8 can only be built in 32 bit)
- `sudo dnf install libgfortran.i686'
- `sudo dnf install glibc-devel.i686'
- `sudo dnf install libnsl.i686'
- `sudo dnf install libX11.i686'
- `sudo dnf install libXdmcp.i686'

## Run:
- `make clean'
- `make astuce'
- `make mad.standard'
