      subroutine pa6xfm(fp, nord, gm, hp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Linear transformation of arguments in a polynomial.                *
* Basic idea: MARYLIE, version 3.0 (routine XFORM).                    *
* Authors:    Liam Healy and Philippo Neri.                            *
* Algorithm:  Christoph Iselin.
* Input:                                                               *
*   FP(*)     (poly)    Homogeneous polynomial to be transformed.      *
*   NORD      (integer) Order of F.                                    *
*   GM(6,6)   (real)    Matrix to transform Z -> GM * Z.               *
* Output:                                                              *
*   HP(*)     (poly)    The result of H = F(GM * Z).                   *
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
      integer i1,i2,i3,i4,i5,i6,j2,j3,j4,j5,j6,l,l1,l2,l3,l4,l5,l6,lp,
     +m1,m2,m3,m4,m5,nord
      double precision fp,gm,hp,tp
      dimension         fp(*), gm(6,6), hp(*)
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
 
*---- For speed reasons, this array was not put into the working space.
      dimension         tp(461)
 
*---- Clear result array.
      do 10 l = ibot6(nord), itop6(nord)
        hp(l) = 0.0
   10 continue
 
*---- Initializations.
      j2 =   6
      j3 =  27
      j4 =  83
      j5 = 209
      j6 = 461
 
*---- First index.
      do 190 i1 = 1, 6
        if (nord .gt. 1) then
          do 110 m1 = ibot6(nord-1), itop6(nord-1)
            tp(m1) = 0.0
  110     continue
 
*---- Second index.
          do 290 i2 = i1, 6
            j2 = j2 + 1
            if (nord .gt. 2) then
              do 210 m2 = ibot6(nord-2), itop6(nord-2)
                tp(m2) = 0.0
  210         continue
 
*---- Third index.
              do 390 i3 = i2, 6
                j3 = j3 + 1
                if (nord .gt. 3) then
                  do 310 m3 = ibot6(nord-3), itop6(nord-3)
                    tp(m3) = 0.0
  310             continue
 
*---- Fourth index.
                  do 490 i4 = i3, 6
                    j4 = j4 + 1
                    if (nord .gt. 4) then
                      do 410 m4 = ibot6(nord-4), itop6(nord-4)
                        tp(m4) = 0.0
  410                 continue
 
*---- Fifth index.
                      do 590 i5 = i4, 6
                        j5 = j5 + 1
                        if (nord .gt. 5) then
                          do 510 m5 = ibot6(nord-5), itop6(nord-5)
                            tp(m5) = 0.0
  510                     continue
 
*---- Sixth index.
                          do 690 i6 = i5, 6
                            j6 = j6 + 1
                            if (fp(j6) .ne. 0.0) then
                              do 620 l6 = 1, 6
                                tp(l6) = tp(l6) + fp(j6) * gm(i6,l6)
  620                         continue
                            endif
  690                     continue
 
*---- Finish fifth index.
                          do 570 l5 = 1, 6
                            if (gm(i5,l5) .ne. 0.0) then
                              lp = lq(lprd6-l5)
                              do 560 m5 = ibot6(nord-5), itop6(nord-5)
                                tp(iq(lp+m5)) = tp(iq(lp+m5)) +
     +                            tp(m5) * gm(i5,l5)
  560                         continue
                            endif
  570                     continue
                        else if (fp(j5) .ne. 0.0) then
                          do 580 l5 = 1, 6
                            tp(l5) = tp(l5) + fp(j5) * gm(i5,l5)
  580                     continue
                        endif
  590                 continue
 
*---- Finish fourth index.
                      do 470 l4 = 1, 6
                        if (gm(i4,l4) .ne. 0.0) then
                          lp = lq(lprd6-l4)
                          do 460 m4 = ibot6(nord-4), itop6(nord-4)
                            tp(iq(lp+m4)) = tp(iq(lp+m4)) +
     +                        tp(m4) * gm(i4,l4)
  460                     continue
                        endif
  470                 continue
                    else if (fp(j4) .ne. 0.0) then
                      do 480 l4 = 1, 6
                        tp(l4) = tp(l4) + fp(j4) * gm(i4,l4)
  480                 continue
                    endif
  490             continue
 
*---- Finish third index.
                  do 370 l3 = 1, 6
                    if (gm(i3,l3) .ne. 0.0) then
                      lp = lq(lprd6-l3)
                      do 360 m3 = ibot6(nord-3), itop6(nord-3)
                        tp(iq(lp+m3)) = tp(iq(lp+m3)) +
     +                    tp(m3) * gm(i3,l3)
  360                 continue
                    endif
  370             continue
                else if (fp(j3) .ne. 0.0) then
                  do 380 l3 = 1, 6
                    tp(l3) = tp(l3) + fp(j3) * gm(i3,l3)
  380             continue
                endif
  390         continue
 
*---- Finish second index.
              do 270 l2 = 1, 6
                if (gm(i2,l2) .ne. 0.0) then
                  lp = lq(lprd6-l2)
                  do 260 m2 = ibot6(nord-2), itop6(nord-2)
                    tp(iq(lp+m2)) = tp(iq(lp+m2)) +
     +                tp(m2) * gm(i2,l2)
  260             continue
                endif
  270         continue
            else if (fp(j2) .ne. 0.0) then
              do 280 l2 = 1, 6
                tp(l2) = tp(l2) + fp(j2) * gm(i2,l2)
  280         continue
            endif
  290     continue
 
*---- Finish first index.
          do 170 l1 = 1, 6
            if (gm(i1,l1) .ne. 0.0) then
              lp = lq(lprd6-l1)
              do 160 m1 = ibot6(nord-1), itop6(nord-1)
                hp(iq(lp+m1)) = hp(iq(lp+m1)) + tp(m1) * gm(i1,l1)
  160         continue
            endif
  170     continue
        else if (fp(i1) .ne. 0.0) then
          do 180 l1 = 1, 6
            hp(l1) = hp(l1) + fp(i1) * gm(i1,l1)
  180     continue
        endif
  190 continue
 
      end
