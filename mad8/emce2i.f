      subroutine emce2i(em, ex, ey, et, sigma)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Convert eigenvectors to internal sigma matrix form.                *
* Input:                                                               *
*   EM(6,6)   (real)    Eigenvector matrix.                            *
*   EX        (real)    Horizontal emittance.                          *
*   EY        (real)    Vertical emittance.                            *
*   ET        (real)    Longitudinal emittance.                        *
* Output:                                                              *
*   SIGMA(6,6)(real)    Beam matrix in internal form.                  *
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
      integer j,k
      double precision em,et,ex,ey,sigma
      dimension         em(6,6), sigma(6,6)
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
 
      do 20 j = 1, 6
        do 10 k = 1, 6
          sigma(j,k) =
     +      ex * (em(j,1) * em(k,1) + em(j,2) * em(k,2)) +
     +      ey * (em(j,3) * em(k,3) + em(j,4) * em(k,4))
          if (stabt) then
            sigma(j,k) = sigma(j,k) +
     +        et * (em(j,5) * em(k,5) + em(j,6) * em(k,6))
          endif
   10   continue
   20 continue
 
      end
