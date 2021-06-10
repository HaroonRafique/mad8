      subroutine hafunc
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Compute the "HARMON" functions; HFUNCT command.                    *
* Attribute:                                                           *
*   DETAIL    (logical) Detail flag.                                   *
*                       False: print summary only.                     *
*                       True:  print also details.                     *
*----------------------------------------------------------------------*
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      integer irg1,irg2,nsup
 
*---- Communication area for current beam line.
      common /rngchr/   linnam, rngnam
      common /rngint/   irg1, irg2, nsup
      common /rnglog/   symm
      save              /rngchr/, /rngint/, /rnglog/
      character         linnam*(mcnam), rngnam*(mcrng)
      logical           symm
      integer lcali,lcatt,lccls,lccmd,lccom,lcdef,lcelm,lcexp,lcfld,
     +lckey,lcseq,lcspl,lcsrc,lcvar,ldbnk,ldkey,lref1,lref2,lsali,lscom,
     +lsdir,lsfld,lsflg,lsnum,lsspl,lbeam,lconsm,ldummy
 
*---- Global reference links.
      common /refer/    lref1,
     +                  lcali, lcatt, lccls, lccmd, lccom, lcdef, lcelm,
     +                  lcexp, lcfld, lckey, lcseq, lcspl, lcsrc, lcvar,
     +                  lbeam, lconsm, ldbnk(4), ldkey(4), ldummy(10),
     +                  lsali, lscom, lsdir, lsfld, lsflg, lsnum, lsspl,
     +                  lref2
      save              /refer/
      integer liftseq, currseq
      common /seqinfi/ liftseq, currseq
      character * (mcnam) sequnam, seqnames
      common /seqinfc/ sequnam, seqnames(mttact)
      integer iprint,nline
      double precision zero
 
      parameter         (zero = 0.0d0)
      character*(*)     title
      logical           detail
 
      parameter         (title = 'HARMON functions.')
 
*---- Retrieve print flag.
      iprint = 1
      detail = .false.
      call utglog(lccmd, 1, 1, detail)
      if (detail) iprint = 2
 
*---- Print page title.
      if (iprint .gt. 0) then
        call prhead('HFUNCT', title, zero, 0, nline, 1)
      endif
 
*---- Compute and print the "HARMON functions".
      call hathin(iprint)
 
*---- Print table of averaged lattice functions.
      if (iprint .gt. 1) call hapave
 
      end
