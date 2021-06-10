      subroutine utbeam(lseq, irg1, irg2, symm, nsup, linnam, rngnam)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Fetch data for a beam line sequence.                               *
* Input:                                                               *
*   LSEQ(1)   (pointer) Beam line sequence bank.                       *
* Output:                                                              *
*   IRG1      (integer) Begin of range index.                          *
*   IRG2      (integer) End of range index.                            *
*   SYMM      (logical) Symmetry flag.                                 *
*   NSUP      (integer) Number of superperiods.                        *
*   LINNAM    (char)    Name of beam line.                             *
*   RNGNAM    (char)    Name of range.                                 *
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
      integer           lseq(*)
      logical           symm
      character*(mcnam) linnam
      character*(mcrng) rngnam
      integer memlen,memmin
      parameter         (memmin =  1600 000)
      parameter         (memlen = 16000 000)
      integer llump,lq,lroot
      double precision dq
 
*---- Memory pool definition.
      common //         fence, lq(mwflt*memlen)
      integer           iq(mwflt*memlen)
      real              fence(2), q(mwflt*memlen)
      dimension         dq(memlen)
      equivalence       (iq(1), q(1), dq(1), lq(9))
      equivalence       (lroot, lq(1)), (llump, lq(2))
      integer msali,msbn,mscom,mscor,msdir,mselm,msf1,msf2,msfld,msflg,
     +mslie,mslnk,msmap,msmon,msnum,msr1,msr2,msref,msrn,mss,msspl,msup,
     +msym
 
*---- Bias for sequence description banks.
      parameter         (msf1 = 1, msr1 = 2, msr2 = 3, msym = 4,
     +                   msup = 5, msf2 = 6, msbn = 7,
     +                   msrn = msbn + mwnam, mss = msrn + 40 / mcwrd)
*     Links for sequence description banks.
      parameter         (msdir =  1, msflg =  2, msali =  3, msfld =  4,
     +                   msnum =  5, mscom =  6, msmap =  9, mslie = 10,
     +                   msspl = 11, mscor = 12, msmon = 13, mselm = 14)
      parameter         (mslnk = 11, msref = 14)
 
*---- Retrieve sequence and range data.
      if (lseq(1) .ne. 0) then
        irg1 = iq(lseq(1)+msr1)
        irg2 = iq(lseq(1)+msr2)
        symm = iq(lseq(1)+msym) .ne. 0
        nsup = iq(lseq(1)+msup)
        call uhtoc(q(lseq(1)+msbn), mcwrd, linnam, mcnam)
        call uhtoc(q(lseq(1)+msrn), mcwrd, rngnam, 40)
      endif
 
      end
