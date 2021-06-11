      subroutine kwput(lkey, nkat)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Fill in a keyword bank.                                            *
* Input:                                                               *
*   LKEY(1)  (pointer)  Bank pointer.                                  *
*   NKAT     (integer)  Number of attributes.                          *
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
      integer ibias,ikat,nkat
      integer           lkey(*)
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
      integer mkdim1,mkdim2,mkdim3,mkf1,mkf2,mkname,mksiz,mktype
 
*---- Bias for keyword attribute groups.
      parameter         (mkf1 = 1, mktype = 2, mkdim1 = 3, mkdim2 = 4,
     +                   mkdim3 = 5, mkf2 = 6, mkname = 7,
     +                   mksiz = mwnam + 6)
      integer iadim1,iadim2,iadim3,iatype,maxat
 
*---- Block for current keyword.
      parameter         (maxat = 100)
      common /keywdi/   iatype(maxat),
     +                  iadim1(maxat), iadim2(maxat), iadim3(maxat)
      common /keywdc/   katnam(maxat)
      save              /keywdi/, /keywdc/
      character*(mcnam) katnam
 
*---- Move attributes to keyword bank.
      ibias = mbat
      do 90 ikat = 1, nkat
        iq(lkey(1)+ibias+mkf1) = 16 * 4 + 2
        iq(lkey(1)+ibias+mktype) = iatype(ikat)
        iq(lkey(1)+ibias+mkdim1) = iadim1(ikat)
        iq(lkey(1)+ibias+mkdim2) = iadim2(ikat)
        iq(lkey(1)+ibias+mkdim3) = iadim3(ikat)
        iq(lkey(1)+ibias+mkf2) = 16 * mwnam + 5
        call uctoh(katnam(ikat), iq(lkey(1)+ibias+mkname), mcwrd, mcnam)
        ibias = ibias + mksiz
   90 continue
 
      end
