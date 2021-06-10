      subroutine lmtilt(nord, tilt, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*    Modify map for elements rotated by TILT.                          *
* Input:                                                               *
*   NORD      (integer) Order desired.                                 *
*   TILT      (real)    Tilt angle.                                    *
* Input/output:                                                        *
*   FP, FM    (map)     Element map to be rotated.                     *
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
      integer i,iftp,isave,j,nord
      double precision c,f1mj,f2mj,fm,fmi1,fmi2,fp,ftm,s,tilt
      dimension         fp(*), fm(6,6)
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
      integer ibot6,itop6,larrp,larrq,lexp6,lind61,lind62,lprd6
 
*---- Bookkeeping tables for polynomials of six variables.
      common /pa6lnk/   ibot6(-6:6), itop6(-6:6), lexp6(6),
     +                  lind61, lind62, larrq, larrp, lprd6
      save              /pa6lnk/
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
 
      dimension         ftm(6,6)
 
      isave = iwork
 
*---- Linear terms.
      c = cos(tilt)
      s = sin(tilt)
      do 10 i = 1, 6
        fmi1 = fm(i,1)
        fm(i,1) = fmi1 * c - fm(i,3) * s
        fm(i,3) = fmi1 * s + fm(i,3) * c
        fmi2 = fm(i,2)
        fm(i,2) = fmi2 * c - fm(i,4) * s
        fm(i,4) = fmi2 * s + fm(i,4) * c
   10 continue
      do 20 j = 1, 6
        f1mj = fm(1,j)
        fm(1,j) = c * f1mj - s * fm(3,j)
        fm(3,j) = s * f1mj + c * fm(3,j)
        f2mj = fm(2,j)
        fm(2,j) = c * f2mj - s * fm(4,j)
        fm(4,j) = s * f2mj + c * fm(4,j)
   20 continue
 
*---- Third order terms.
      if (nord .ge. 3) then
 
*---- Allocate working space.
        iftp  = iwork
        iwork = iftp + itop6(min(nord,4))
        if (iwork .gt. nwork) then
          call mzwork(0, dq(1), dq(iwork+1), 2)
          nwork = iwork
        endif
        call m66one(ftm)
        ftm(1,1) = c
        ftm(1,3) = s
        ftm(3,1) = -s
        ftm(3,3) = c
        ftm(2,2) = c
        ftm(2,4) = s
        ftm(4,2) = -s
        ftm(4,4) = c
        call pa6xfm(fp, 3, ftm, dq(iftp+1))
        call pa6cpy(dq(iftp+1), 3, fp)
 
*---- Fourth order terms.
        if (nord .ge. 4) then
          call pa6xfm(fp, 4, ftm, dq(iftp+1))
          call pa6cpy(dq(iftp+1), 4, fp)
        endif
      endif
 
*---- Drop working storage.
      iwork = isave
 
      end
