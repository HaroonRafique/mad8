      subroutine utgtyp(lcmd, itype)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Fetch types of command attributes (zero for unset ones).           *
* Input:                                                               *
*   LCMD(1)   (pointer) Command to be used.                            *
* Output:                                                              *
*   ITYPE(*)  (real)    Vector of type codes to be filled.             *
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
      integer ibias,icat,itype
      integer           lcmd(*)
      dimension         itype(*)
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
      integer mbat,mbecls,mbelie,mbemap,mbfrm,mbln,mbnam,mbpr,mbsp
 
*---- Bias for bank descriptor words.
      parameter         (mbfrm  = 1, mbnam  = 2, mbln   = 3,
     +                   mbpr   = 4, mbsp   = 5, mbat   = 6)
      parameter         (mbemap = 1, mbelie = 2, mbecls = 3)
      integer mcf1,mcf2,mcsiz,mctyp,mcval
 
*---- Bias for command attribute groups.
      parameter         (mcf1 = 1, mctyp = 2, mcf2 = 3, mcval = 4,
     +                   mcsiz = mwnam + 3)
 
      ibias = mbat
      do 90 icat = 1, iq(lcmd(1)+mbat)
        if (mod(iq(lcmd(1)+ibias+mctyp),10) .ne. 0) then
          itype(icat) = iq(lcmd(1)+ibias+mctyp) / 10
        else
          itype(icat) = 0
        endif
        ibias = ibias + mcsiz
   90 continue
 
      end
