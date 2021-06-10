      subroutine tmtrak(ek, re, te, orb1, orb2)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Track orbit and change reference for RE matrix.                    *
* Input:                                                               *
*   EK(6)     (real)    Kick on orbit.                                 *
*   RE(6,6)   (real)    Transfer matrix before update.                 *
*   TE(6,6,6) (real)    Second order terms.                            *
*   ORB1(6)   (real)    Orbit before element.                          *
* Output:                                                              *
*   ORB2(6)   (real)    Orbit after element.                           *
*   RE(6,6)   (real)    Transfer matrix after update.                  *
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
      integer i,k,l
      double precision ek,orb1,orb2,re,sum1,sum2,te,temp
      dimension         ek(6), re(6,6), te(6,6,6), orb1(6), orb2(6)
      double precision cofact,optflt
 
*---- Option flags.
      common /optflt/   optflt(10)
      equivalence       (cofact, optflt( 1))
      common /optint/   optint(10)
      integer           optint
      integer           icmdfl, ideffl, iexpfl, ikeyfl, ilinfl
      equivalence       (icmdfl, optint( 1)), (ideffl, optint( 2))
      equivalence       (iexpfl, optint( 3)), (ikeyfl, optint( 4))
      equivalence       (ilinfl, optint( 5))
      common /optlog/   optflg(20), optcon(5)
      logical           optflg, optcon
      logical           debug,  double, echo,   inter,  trace,  verify,
     +                  warn,   info,   sympl,  rbarc, ereset, bborbit
      logical           reset,  tell
      equivalence       (debug,  optflg( 1)), (double, optflg( 2))
      equivalence       (echo,   optflg( 3)), (inter,  optflg( 4))
      equivalence       (trace,  optflg( 5)), (verify, optflg( 6))
      equivalence       (warn,   optflg( 7)), (info,   optflg( 8))
      equivalence       (sympl,  optflg( 9)), (rbarc,  optflg(10))
      equivalence       (ereset,  optflg(11)),(bborbit,optflg(12))
      equivalence       (reset,  optcon( 1)), (tell,   optcon( 2))
      save              /optflt/, /optint/, /optlog/
 
      dimension         temp(6)
 
      do 30 i = 1, 6
        sum2 = ek(i)
        do 20 k = 1, 6
          sum1 = 0.0
          do 10 l = 1, 6
            sum1 = sum1 + te(i,k,l) * orb1(l)
   10     continue
          sum2 = sum2 + (re(i,k) + sum1) * orb1(k)
          re(i,k) = re(i,k) + sum1 + sum1
   20   continue
        temp(i) = sum2
   30 continue
 
      call ucopy(temp, orb2, 6*mwflt)
 
*---- Symplectify transfer matrix.
      if (sympl) call tmsymp(re)
 
      end
