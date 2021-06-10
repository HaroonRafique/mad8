MAD-8, DOOM, c6t, and associated programs
===========================================

Acknowledgement: 

    This file is a direct copy of Hans Grote's madpackage.Read_Me file of
    February 13, 2004 (also contained in this distribution) with some minor
    additions which describe SLAC modifications to MAD-8 (v8.51/15).  Hans
    gets the credit for everything in this file that's right ... I get the
    blame for anything that's wrong!

 1. This Read_Me file, and the compressed source madpackage_SLAC.tgz, may be
    found on the web at:

       ftp://ftp.slac.stanford.edu/groups/nlc/mad8.51.15

    If you have afs access to SLAC, the directory is:

       /afs/slac.stanford.edu/public/groups/nlc/mad8.51.15

    You may also ftp anonymously to ftp.slac.stanford.edu (user: anonymous,
    password: your email address), then

       cd groups/nlc/mad8.51.15
       get madpackage_SLAC.tgz

    To uncompress:

       tar -x -z -f madpackage_SLAC.tgz ... Linux
       gtar -xzf madpackage_SLAC.tgz    ... unix

    which will create a directory madpackage (total space < 6 Mbytes).  To
    ensure that you can execute the scripts:

       chmod a+x mad
       chmod a+x sxf
       chmod a+x mad_c6t

    Documentation for MAD-8 (User's Guide, Physicist's Guide, and Programmer's
    Guide) can be obtained via the MAD-8 web site:

       http://mad.web.cern.ch/mad/mad8web/mad8.html

 2. The Makefile is prepared for Linux and should work when all files are in
    the same directory.  With slight modifications it will actually work on
    other Unix systems as well: there are flags for HP (10.20), aix, osf1, and
    sun.  Hans recommends to use gmake except under Linux; I find that, here at
    SLAC, make seems to work just fine.

    The following libraries are required in addition to the source present:

    a. the three CERN libraries: libmathlib.a, libpacklib.a, and libkernlib.a
       (available from CERN ... see http://wwwinfo.cern.ch/asd/index.html)
    b. the standard C libraries -lnsl -lm -lc 
    c. the X11 library libX11.a (only for plotting) which resides typically in
       /usr/lib or in /usr/lib/X11R6 or similar

 3. Prior to making any program you must make the code management tool astuce:

       make astuce

 4. MAD versions: there are four MAD-8 versions available:

    a. standard (with and without plotting)
    b. with DOOM interface (with and without plotting)

    The DOOM version without plotting is needed for c6t, SXF_in and SXF_ex.
    You may skip steps 5 and 6 accordingly.

 5. make doom_newdb

 6. make an initial DOOM database from file doom.start:

       doom_newdb doom.start doom.initdb

 7. Depending on your choice:

       make mad.standard
       make mad.standard.noplot
       make mad.doom
       make mad.doom.noplot
       make mad.windows (extracts source code for Windows systems)

 8. If you need the c6t (MAD-Sixtrack converter):

       make c6t

 9. SXF file converters:

       make SXF_in
       make SXF_ex

    will make the corresponding programs.

10. get_corr3 program (calculates LHC triplet corrector settings):

       make get_corr3

11. MAD execution: the mad script contained in the package allows the four
    execution modes:

       mad               (this is the standard plot version)
       mad -noplot       (standard version without plots)
       mad -doom         (doom version with plot)
       mad -doom -noplot (doom version without plot)

    The doom versions will copy the initial database doom.initdb to a database
    doom.db which will then be filled.  This means that if you want to save a
    database before executing mad -doom once more you have to move the database
    filled previously to some other name.

12. MAD-Sixtrack converter:

       mad_c6t (uses a database doom.db to make the Sixtrack input files)

13. SXF file converters:

       sxf -in sxf_file      (create doom.db from sxf_file)
       sxf -ex sequence_name (create sxf.ex from doom.db for sequence_name)
   
14. Test job (1): a typical run with screen plot

       mad
       call "elettra.mad" (at the prompt)
       (answer the questions)

    To exit from a plot on the screen, type <enter> in the plot window.  The
    x_axis with tick marks etc. may be missing on the screen, but will always
    be OK in the PostScript file.

    If you run instead

       mad < elettra.mad

    there is no plot on the screen, but you receive automatically a PostScript
    file mad.ps containing the plots.

15. Test job (2): a typical run to make Sixtrack input files:

       mad -doom -noplot < elettra.mad
       mad_c6t

16. Test job (3): a typical write and read SXF files:

       mad -doom -noplot < elettra.mad
       sxf -ex RING

    will read sequence RING from doom.db and make a file ex.sxf

       mv ex.sxf in.sxf
       sxf -in in.sxf

    will read the SXF format file in.sxf and make a data doom.db.

17. Some additional files are included for use with the SLAC version of
    MAD-8.51:

       mad8_user.pdf (PDF version of User's Guide)
       mad8_phys.pdf (PDF version of Physicist's Guide)
       mad8_prog.pdf (PDF version of Programmer's Guide)
       mad8_lcav.pdf (User's Guide section describing the LCAVITY element)

    plus wakefield definition files for the superconducting 9-cell ILC/TESLA
    accelerating cavity:

       ilc.lwake.sr.data           (longitudinal wakefield for short bunches)
       ilc.lwake.sr.longbunch.data (longitudinal wakefield for long  bunches)
       ilc.twake.sr.data           (transverse   wakefield for short bunches)
       ilc.twake.sr.longbunch.data (transverse   wakefield for long  bunches)

Mark Woodley (SLAC), November 2, 2006
