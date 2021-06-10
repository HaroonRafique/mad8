MAD-8, DOOM, c6t, and associated programs
===========================================

1. This Read_Me file, and the compressed source madpackage.tgz are in

   http://cern.ch/Hans.Grote/src/mad_new/

   The actual directory (for afs access) is
   /afs/cern.ch/user/h/hansg/public/www/src/mad_new

   BEFORE YOU READ ANY FURTHER:

   The latest executable binary for Linux,
   and the corresponding mad8.dict are to be found in

   http://cern.ch/Hans.Grote/dist/mad_new/

   there is a high probability they will work on a Linux system.
   Do not forget ln -s mad8.dict dict

   The previous executable binary for SunOS, and OSF1,
   and the corresponding mad8.dict are kept at the same place, but
   will not be updated any further.

   INSTALL THE LATEST VERSION FROM THE SOURCE:

   To uncompress, run tar under Linux, gtar elsewhere:

   tar -x -z -f madpackage.tgz

   which will create a directory madpackage (total space ca. 4 Mbytes).
   To make sure you can execute the scripts:
   chmod a+x mad
   chmod a+x sxf
   chmod a+x mad_c6t

2. The Makefile is prepared for Linux and should work when all files
   are in the same directory. With slight modifications it will
   actually work on other Unix systems as well: there are flags for HP
   (10.20), aix, osf1, and sun. I recommend to use gmake except under Linux.

   The following libraries are required in addition to the source present:
a. the three CERN libraries 
   libmathlib.a, libpacklib.a, libkernlib.a
   which can be obtained from CERN (see
   http://wwwinfo.cern.ch/asd/index.html )
b. the standard C libraries -lm -lc 
c. the X11 library libX11.a (only for plotting) which resides
   typically in /usr/lib or in /usr/lib/X11R6 or similar.
d. The MAD-8 User's Manual can be found from the MAD home page
   http://cern.ch/mad

3. Prior to making any program you must make the code management tool
   astuce:
   make astuce

4. MAD versions:
   there are four MAD-8 versions available: standard with and without
   plotting, with DOOM interface with and without plotting. The DOOM
   version without plotting is needed for c6t, SXF_in and SXF_ex.
   You may skip steps 5 and 6 accordingly.

5. make doom_newdb

6. doom_newdb doom.start doom.initdb
   makes an initial DOOM database from file doom.start

7. Depending on your choice:
   make mad.standard
   make mad.standard.noplot
   make mad.doom
   make mad.doom.noplot

8. If you need the c6t (MAD-Sixtrack converter):
   make c6t

9. SXF file converters:
   make SXF_in
   make SXF_ex
   will make the correspoding programs.

9.a get_corr3 program (calculates LHC triplet corrector settings):
   make get_corr3

10.MAD execution:
   the mad script contained in the package allows the four execution
   modes:
   mad               (this is the standard plot version)
   mad -noplot       (standard version without plots)
   mad -doom         (doom version with plot)
   mad -doom -noplot (doom version without plot)

   The doom versions will copy the intial database doom.initdb
   to a database doom.db which will then be filled. This means
   that if you want to save a database before executing mad -doom
   once more you have to move the database filled previously to some
   other name.

10.MAD-Sixtrack converter:
   mad_c6t
   which will use a database doom.db to make the Sixtrack input files.

11.SXF file converters:
   sxf -in sxf_file
   will create a database doom.db from the sxf_file
   sxf -ex sequence_name
   will make a file sxf.ex from doom.db for sequence sequence_name

12.Test jobs
a. A typical run with screen plot is as follows:

   mad

   at the prompt,

   call "elettra.mad"

   and answer the questions.
   To exit from a plot on the screen, type <enter> in the plot window.
   The x_axis with tick marks etc. may be missing on the screen, but
   will always be OK in the Postscript file.

   If you run instead

   mad < elettra.mad

   there is no plot on the screen, but you receive automatically a
   Postscript file mad.ps containing the plots.

b. A typical run to make Sixtrack input files:

   mad -doom -noplot < elettra.mad
   mad_c6t

c. A typical write and read SXF files:

   mad -doom -noplot < elettra.mad
   sxf -ex RING

   will read sequence RING from doom.db and make a file ex.sxf

   mv ex.sxf in.sxf
   sxf -in in.sxf

   will read the SXF format file in.sxf and make a data doom.db.

HG  13.2.2004

