      subroutine dcstrg(ilink, idata, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode string and lift string bank.                                *
* Input:                                                               *
*   LDCBNK   /DCLINK/   Data bank pointer.                             *
*   ILINK    (integer)  Bias for pointer to sub-bank.                  *
*   IDATA    (integer)  Bias for data block.                           *
* Output:                                                              *
*   EFLAG    (logical)  Error flag.                                    *
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
      integer idata,ileng,ilink,nd
      logical           eflag
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
      integer mcf1,mcf2,mcsiz,mctyp,mcval
 
*---- Bias for command attribute groups.
      parameter         (mcf1 = 1, mctyp = 2, mcf2 = 3, mcval = 4,
     +                   mcsiz = mwnam + 3)
      integer ldcatt,ldcbnk,ldcfrm,ldckey,ldclin
 
*---- Local links for decoder.
      common /dclink/   ldcatt, ldcbnk, ldcfrm, ldckey, ldclin
      save              /dclink/
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
 
      character*(mcstr)   string
 
      eflag = .false.
      call rdword(string, ileng)
 
*---- Drop any old string bank.
      ldcatt = lq(ldcbnk-ilink)
      if (ldcatt .ne. 0) then
        call mzdrop(0, ldcatt, '.')
        iq(ldcbnk+idata+mctyp) = 10 * mtstr
      endif
 
*---- If string is not empty, lift string bank and return results.
      if (ileng .gt. 0) then
        nd = (ileng - 1) / mcwrd + 1
        call mzbook(2, ldcatt, ldcbnk, -ilink, 'STRG', 0, 0, nd, 5, 0)
        call uctoh(string, iq(ldcatt+1), mcwrd, ileng)
        iq(ldcbnk+idata+mctyp) = 10 * mtstr + 1
        iq(ldcbnk+idata+mcval) = ileng
      endif
 
      end
