      subroutine ladpu2(nord, fp, fm, gp, gm, tp, tm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Purify the matrix part of a dynamic Lie-algebraic map.             *
* Source:     MARYLIE, version 3.0 (routine DPUR2).                    *
* Input:                                                               *
*   NORD      (integer) Order of the map F (at most 4).                *
*   FP, FM    (map)     Original map, unchanged.                       *
* Output:                                                              *
*   GP, GM    (map)     Purified map.                                  *
*   TP, TM    (map)     Purifying map, i. e. G = T*F*T**(-1).          *
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
      integer i,j,nord
      double precision aieig,diff,fm,fp,gm,gp,reeig,tm,tol,tp
      dimension         fp(*), fm(6,6), gp(*), gm(6,6), tp(*), tm(6,6)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
 
      parameter         (tol = 1.0d-8)
      dimension         reeig(6), aieig(6)
 
*---- Find the map T = A2.
      call ladeig(fm, reeig, aieig, tm)
      if (error) go to 999
 
*---- Go to Floquet variables.
      call pa6clr(tp, -nord)
      call lmsand(nord, tp, tm, fp, fm, gp, gm)
 
*---- If eigenvalues are off unit circle, print warning message.
      do 20 i = 1, 6
        if (abs(reeig(i)**2 + aieig(i)**2 - 1.0) .gt. tol) then
          write (msg, 910)
          do 10 j = 1, 6
            diff = sqrt(reeig(j)**2 + aieig(j)**2) - 1.0
            write (msg(j+2), 920) reeig(j), aieig(j), diff
   10     continue
          call aawarn('LADPU2', 8, msg)
          go to 999
        endif
   20 continue
 
  910 format('Eigenvalues not on unit circle:'/
     +       6x,'Real',12x,'Imaginary',5x,'Error')
  920 format(2f16.8,1p,e18.6)
 
  999 end
