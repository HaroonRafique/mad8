      subroutine pa6prt(f, nord, iprnt)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print polynomials representing a map.                              *
* Input:                                                               *
*   F(*)                Coefficients of polynomial.                    *
*   NORD      (integer) All monomials up to order NORD are printed.    *
*   IPRNT     (integer) Logical unit number.                           *
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
      integer iprnt,j,jl,jord,k,nl,nord
      double precision f,tol
      dimension         f(*)
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
 
      parameter         (tol = 1.0d-8)
      integer           il(209)
 
*---- Polynomials F(1) through F(norder):
*     Check for any non-zero coefficient.
      do 90 jord = 1, nord
        nl = 0
        do 70 j = ibot6(jord), itop6(jord)
          if (abs(f(j)) .gt. tol) then
            nl = nl + 1
            il(nl) = j
          endif
   70   continue
 
*---- There is at least one coefficient to be printed.
        if (nl .gt. 0) then
          write (iprnt, 910) jord
          write (iprnt, 920) ((iq(lexp6(k)+il(jl)), k = 1, 6),
     +                        f(il(jl)), jl = 1, nl)
        endif
   90 continue
 
  910 format(' Terms of order ',i1,':')
  920 format(4(' C',6i1,' = ',1p,e14.6:5x))
 
      end
