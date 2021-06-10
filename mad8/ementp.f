      subroutine ementp(iloc, elmnam, idisk, sigma, suml)
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   TAPE option of ENVELOPE command.                                   *
* Input:                                                               *
*   ILOC      (integer) Position code:                                 *
*                       1 = Beginning of system.                       *
*                       2 = After an element.                          *
*                       3 = Summary at end of system.                  *
*   ELMNAM    (char)    Name associated with current element.          *
*   IDISK     (integer) Logical unit for output                        *
*   SIGMA(6,6)(real)    Beam matrix in internal form.                  *
*   SUML      (real)    Accumulated length.                            *
*----------------------------------------------------------------------*
* Created:  18-MAR-1999, M. Woodley (SLAC)                             *
*   Add tape file output for ENVELOPE command                          *
* Modified: 16-APR-1999, M. Woodley (SLAC)                             *
*   Add INITIAL record                                                 *
*----------------------------------------------------------------------*
      implicit none
      integer i, iloc, idisk
      character *(*) elmnam
      double precision sigma(6,6), suml
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
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
 
      if (iloc .eq. 1) then
*---- Begin of system: Write header record.
        call tphead(idisk, 'ENVELOPE')
        lcelm = 0
        call tpelem('INITIAL', idisk)
        write (idisk, 910)
     +    (sigma(1,i), i = 1, 6),
     +    (sigma(2,i), i = 1, 6),
     +    (sigma(3,i), i = 1, 6),
     +    (sigma(4,i), i = 1, 6),
     +    (sigma(5,i), i = 1, 6),
     +    (sigma(6,i), i = 1, 6), suml
      else if (iloc .eq. 3) then
*---- End of system: Write summary record.
      else
*---- Exit of element: Write data record.
        call tpelem(elmnam, idisk)
        write (idisk, 910)
     +    (sigma(1,i), i = 1, 6),
     +    (sigma(2,i), i = 1, 6),
     +    (sigma(3,i), i = 1, 6),
     +    (sigma(4,i), i = 1, 6),
     +    (sigma(5,i), i = 1, 6),
     +    (sigma(6,i), i = 1, 6), suml
      endif
 
  910 format(1p,6e16.9/6e16.9/6e16.9/6e16.9/6e16.9/7e16.9)
 
      end
