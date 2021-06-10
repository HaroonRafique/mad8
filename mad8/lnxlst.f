      subroutine lnxlst(idir)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Advance LIST "LLNCAL" to next member.                              *
* Input:                                                               *
*   IDIR      (integer) Code for direction of travel.                  *
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
      integer mlact,mlf1,mlf2,mlfm,mlfree,mlhd,mlnxt,mlprv,mlref,mlrep,
     +mlsiz,mltyp
 
*---- Bias for beam line list information.
      parameter         (mlfm = mbat + 1, mlhd = mbat + 2,
     +                   mlf1 = mbat + 3, mlf2 = mbat + 4,
     +                   mlfree = mbat + 4)
 
*---- Bias for beam line list cells.
      parameter         (mltyp = 1, mlprv = 2, mlnxt = 3, mlrep = 4,
     +                   mlref = 5, mlact = 6, mlsiz = 6)
      integer llnact,llnbnk,llncal,llneat,llnedr,llnefl,llnesq,llnhed,
     +llnrls,llnrsq,llnsup,llntmp,llnxls,llnxsq
 
*---- Link area for beam line handler.
      common /lnlink/   llnbnk, llnrls, llnrsq, llnsup,
     +                  llnact, llncal, llnhed, llnxls, llnxsq,
     +                  llnesq, llnedr, llneat, llntmp(4), llnefl
      save              /lnlink/
      integer icur,idir,inext
 
*---- Is repeat count for current member exhausted?
      icur = iq(llncal+mlf1)
      iq(llncal+mlf2) = iq(llncal+mlf2) - 1
      if (iq(llncal+mlf2) .le. 0) then
        icur = iq(llncal+icur+mlnxt)
 
*---- If list header, test repeat count of list.
  100   if (iq(llncal+icur+mltyp) .le. 3) then
          if (iq(llncal+icur+mltyp) .ne. 1) then
            iq(llncal+icur+mlrep) = iq(llncal+icur+mlrep) - 1
            if (iq(llncal+icur+mlrep) .le. 0) then
              icur = iq(llncal+icur+mlref)
            endif
          endif
          icur = iq(llncal+icur+mlnxt)
          go to 100
 
*---- If call to sublist, move down one level.
        else if (iq(llncal+icur+mltyp) .le. 5) then
          inext = iq(llncal+icur+mlref)
          iq(llncal+inext+mlrep) = abs(iq(llncal+icur+mlrep))
          iq(llncal+inext+mlref) = icur
          icur = iq(llncal+inext+mlnxt)
          go to 100
        endif
 
*---- Otherwise store current member and repeat count.
        iq(llncal+mlf1) = icur
        iq(llncal+mlf2) = abs(iq(llncal+icur+mlrep))
      endif
 
*---- Fill in dummy line from list item.
      inext = mlfree + mlsiz
      iq(llncal+inext+mltyp) = iq(llncal+icur+mltyp)
      iq(llncal+inext+mlrep) = idir
      iq(llncal+inext+mlref) = iq(llncal+icur+mlref)
      iq(llncal+inext+mlact) = iq(llncal+icur+mlact)
 
      end
