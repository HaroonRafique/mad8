
# Makefile for madpackage - check the paths for the libraries !

# From /afs/cern.ch/user/h/hansg/public/mad/export/linux/
#  madpackage.tgz (Jul 25 02:10)
#
# 27-SEP-2000, M. Woodley (SLAC)
#    Define a third MAD flavor: mad.accel[.noplot]; add SLAC variants of Linux,
#    Sun, and AIX builds; add extraction of source for Windows NT (only the
#    "noplot" version is available for NT (X11 is not available on NT))

LOPTS=-m32

# Linux (at SLAC):
#  SYS=linux
#  FCOMP=f77
#  FCOPTS=-Wno-globals -O3
#  CERN=/cern/pro/lib
#  Xlib=/usr/X11R6/lib
#  LIBS=$(CERN)/libmathlib.a $(CERN)/libpacklib.a $(CERN)/libkernlib.a \
#       $(Xlib)/libX11.a -lnsl -lm -lc
#  ASTLIB=
#  GCC_FLAGS=-g -ansi -Wall -D_LINUX_

# Sun (at SLAC):
# SYS=sun
# FCOMP=f77
# FCOPTS= -O
# CERN=/cern/pro/lib
# Xlib=/usr/openwin/lib
# USR=/usr/lib
# LIBS=-L$(CERN) -L$(Xlib) -L$(USR) \
#      -lmathlib -lpacklib -lkernlib -lX11 -lnsl -lsocket -lm -lc
# ASTLIB=
# GCC_FLAGS=-ansi -Wall

# AIX (at SLAC):
# SYS=aix
# FCOMP=xlf
# FCOPTS=-qcharlen=1024 -O
# CERN=/cern/pro/lib
# Xlib=/usr/local/lib/X11R5
# LIBS=-L$(CERN) -lpacklib -lmathlib -L$(Xlib) -lX11
# ASTLIB=
# GCC_FLAGS=-O

# Linux (at CERN):
 SYS=linux
# there are other compilers, e.g. g77
# FCOMP=f77
 FCOMP=ifort
 FCOPTS=-O2 -m32 -fno-second-underscore -funroll-loops
# FCOMP=gfortran
# FCOPTS=-O2 -m32 -c -pipe -fno-second-underscore -fall-intrinsics -fno-range-check -fmax-stack-var-size=65536 -Jbuild/Linux64_gfortran -D_STATIC -D_GCC -D_GXX -D_GFORTRAN -D_LINUX -D_ONLINE -D_USEGC -DSTATIC=static 
# CERN=/cern/new/lib
# Xlib=/usr/X11R6/lib
# LIBS=$(CERN)/libmathlib.a $(CERN)/libpacklib.a $(CERN)/libkernlib.a \
#      $(Xlib)/libX11.a -lm -lc
 LIBS= -L/extra2/git/madX-SC/lib32 -lX11 -L/usr/lib/cernlib/2006/lib/ -lmathlib -lpacklib -lkernlib /usr/lib/gcc/x86_64-redhat-linux/10/32/libgfortran.a -lquadmath
#/sw/lib/gcc4.2/lib/libgfortran.a
 ASTLIB=
# GCC_FLAGS=-g -ansi -Wall -D_SWAP_ -D_LINUX_
 GCC_FLAGS=-O -c -g -m32 -funroll-loops

# HP (at CERN):
# SYS=hp
# FCOMP=fort77
# FCOPTS=+ppu
# CERN=/cern/pro/lib
# Xlib=/usr/lib/X11R6
# LIBS=$(CERN)/libmathlib.a $(CERN)/libpacklib.a $(CERN)/libkernlib.a \
#      $(Xlib)/libX11.a -lm -lc
# ASTLIB=$(CERN)/libkernlib.a
# GCC_FLAGS=-ansi -Wall

# AIX (at CERN):
# SYS=aix
# FCOMP=f77
# FCOPTS=-qextname -O2
# CERN=/cern/pro/lib
# Xlib=/usr/lib
# LIBS=$(CERN)/libmathlib.a $(CERN)/libpacklib.a $(CERN)/libkernlib.a \
#      $(Xlib)/libX11.a -lm -lc
# ASTLIB=
# GCC_FLAGS=-ansi -Wall

# OSF1 (at CERN):
# SYS=osf1
# FCOMP=f77
# FCOPTS=-O4 -v -tune ev5 -align dcommons -unroll 7
# CERN=/cern/pro/lib
# Xlib=/usr/lib
# LIBS=$(CERN)/libmathlib.a $(CERN)/libpacklib.a $(CERN)/libkernlib.a \
#      $(Xlib)/libX11.a -lm -lc
# ASTLIB=
# GCC_FLAGS=-ansi -Wall -D_swap_

# Sun (at DESY (courtesy W. Decking)):
# SYS=sun
# FCOMP=f77
# FCOPTS= -s -O
# CERN=/cern/pro/lib
# Xlib=/usr/lib/X11
# USR=/usr/lib
# LIBS=-L$(CERN) -L$(Xlib) -L$(USR) \
#      -lmathlib -lpacklib -lkernlib -lX11 -lsocket -lnsl -lm -lc
# ASTLIB=$(CERN)/libkernlib.a
# GCC_FLAGS=-ansi -Wall

# MAC OS-X (at CERN):
ifeq ($(OSTYPE),darwin)
 SYS=linux
# gfortran required by CERNLIB
 FCOMP=g95
 FCOPTS=-O2 -fno-second-underscore -funroll-loops
# CERN=/cern/new/lib
# Xlib=/usr/X11R6/lib
# LIBS=$(CERN)/libmathlib.a $(CERN)/libpacklib.a $(CERN)/libkernlib.a \
#      $(Xlib)/libX11.a -lm -lc
  LIBS= -lX11 -lX11 -L/usr/lib/cernlib/2006/lib/ -lmathlib -lpacklib #-L/sw/lib/gcc4.2/lib/ -lgfortran
 ASTLIB=
# GCC_FLAGS=-g -ansi -Wall -D_SWAP_ -D_LINUX_
 GCC_FLAGS=-O -c -g -funroll-loops -I /usr/X11R6/include/
 LOPTS=
endif

astuce: astuce.f
	$(FCOMP) $(FCOPTS) -o astuce astuce.f $(ASTLIB)

doom.o: doom.c doom.h doomex.h
	gcc $(GCC_FLAGS) -c -o doom.o doom.c

doom_newdb: doom_newdb.c doom.o
	gcc $(GCC_FLAGS) -o doom_newdb doom_newdb.c doom.o -lm -lc

gxx11c.o: gxx11c.c
	gcc $(GCC_FLAGS) -c gxx11c.c

gxx11.o: gxx11.f
	$(FCOMP) -c $(FCOPTS) gxx11.f

gxx11.f: gxx11.ss
	astuce -s gxx11.ss -f gxx11.f -d fortran,x11

gxx11c.c: gxx11.ss
	astuce -s gxx11.ss -f gxx11c.c -d c

mad.doom: mad.doom.o doom.o gxx11.o gxx11c.o
	$(FCOMP) $(LOPTS) -o mad.doom \
		mad.doom.o doom.o gxx11.o gxx11c.o $(LIBS)
	mv mad.doom mad8d

mad.doom.o: mad.doom.f
	$(FCOMP) $(FCOPTS) -c -o mad.doom.o mad.doom.f

mad.doom.f: mad8.ss
	astuce -s mad8.ss -f mad.doom.f -d $(SYS),unix,big,x11,doom

mad.doom.noplot: mad.doom.noplot.o doom.o
	$(FCOMP) $(LOPTS) -o mad.doom.noplot \
		mad.doom.noplot.o doom.o $(LIBS)
	mv mad.doom.noplot mad8dnp

mad.doom.noplot.o: mad.doom.noplot.f
	$(FCOMP) $(FCOPTS) -c -o mad.doom.noplot.o mad.doom.noplot.f

mad.doom.noplot.f: mad8.ss
	astuce -s mad8.ss -f mad.doom.noplot.f -d $(SYS),unix,big,noplot,doom

mad.standard: mad.standard.o gxx11.o gxx11c.o
	$(FCOMP) $(LOPTS) -o mad.standard \
		mad.standard.o gxx11.o gxx11c.o $(LIBS)
	mv mad.standard mad8s

mad.standard.o: mad.standard.f
	$(FCOMP) $(FCOPTS) -c -o mad.standard.o mad.standard.f

mad.standard.f: mad8.ss
	astuce -s mad8.ss -f mad.standard.f -d $(SYS),unix,big,x11,standard

mad.standard.noplot: mad.standard.noplot.o
	$(FCOMP) $(LOPTS) -o mad.standard.noplot \
		mad.standard.noplot.o $(LIBS)
	mv mad.standard.noplot mad8snp

mad.standard.noplot.o: mad.standard.noplot.f
	$(FCOMP) $(FCOPTS) -c -o mad.standard.noplot.o mad.standard.noplot.f

mad.standard.noplot.f: mad8.ss
	astuce -s mad8.ss -f mad.standard.noplot.f \
	       -d $(SYS),unix,big,noplot,standard

mad6t.f: mad6t.ss
	astuce -s mad6t.ss -f mad6t.f -d dummy

mad6t.o: mad6t.f
	$(FCOMP) $(FCOPTS) -c -o mad6t.o mad6t.f

mad6t: mad6t.o doom.o
	$(FCOMP) $(LOPTS) -o mad6t mad6t.o doom.o $(LIBS) -lm -lc

SXF_in: SXF_in.c doom.o
	gcc $(GCC_FLAGS) -o SXF_in SXF_in.c doom.o -lm -lc

SXF_ex: SXF_ex.c doom.o
	gcc $(GCC_FLAGS) -o SXF_ex SXF_ex.c doom.o -lm -lc

mad.accel: mad.accel.o gxx11.o gxx11c.o
	$(FCOMP) $(LOPTS) -o mad.accel \
		mad.accel.o gxx11.o gxx11c.o $(LIBS)
	mv mad.accel mad8a

mad.accel.o: mad.accel.f
	$(FCOMP) $(FCOPTS) -c -o mad.accel.o mad.accel.f

mad.accel.f: mad8.ss
	astuce -s mad8.ss -f mad.accel.f \
	       -d $(SYS),unix,standard,accel,x11,bigplot

mad.accel.noplot: mad.accel.noplot.o
	$(FCOMP) $(LOPTS) -o mad.accel.noplot \
		mad.accel.noplot.o $(LIBS)
	mv mad.accel.noplot mad8anp

mad.accel.noplot.o: mad.accel.noplot.f
	$(FCOMP) $(FCOPTS) -c -o mad.accel.noplot.o mad.accel.noplot.f

mad.accel.noplot.f: mad8.ss
	astuce -s mad8.ss -f mad.accel.noplot.f \
	       -d $(SYS),unix,standard,accel,noplot

mad.NT.f: mad8.ss
	astuce -s mad8.ss -f mad.accel.NT.f \
	       -d wnt,osf1,standard,accel,bigplot
	astuce -s gxx11.ss -f gxx11.NT.f -d fortran,x11,noterm

clean:
	rm -f *.c
	rm -f *.f
	rm -f *.o
	rm -f core* print mad.ps epio71
	rm -f *~
