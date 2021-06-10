      subroutine lnmake(lline, lkey)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Build beam line or list bank.                                      *
*   Uses the pointers to current keyword and current command.          *
* Input:                                                               *
*   LKEY(1)  (pointer)  Keyword pointer.                               *
* Output:                                                              *
*   LLINE(1) (pointer)  Generated beam line list bank.                 *
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
      integer mli
      integer           lline(*), lkey(*)
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
      integer mxals,mxcls,mxdef,mxdrp,mxknw,mxlmp,mxmod,mxord
 
*---- Expression marker bits.
      parameter         (mxdrp = 1, mxdef = 2, mxord = 3,
     +                   mxcls = 4, mxals = 5, mxlmp = 6,
     +                   mxmod = 7, mxknw = 8)
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
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      parameter         (mli = mlfree + 100 * mlsiz)
 
*---- Lift bank, link it to keyword, and mark it as modified.
      call mzbook(2, lline, lkey, -1, 'BLST', 3, 1, mli, 7, 0)
      call aamark('LNMAKE', lline)
 
*---- Store bank description.
      iq(lline(1)+mbfrm) = 16 * 5 + 2
      iq(lline(1)+mbnam) = 0
      iq(lline(1)+mbln) = lintok
      iq(lline(1)+mbpr) = iq(lkey(1)+mbpr)
      iq(lline(1)+mbsp) = iq(lkey(1)+mbsp)
      iq(lline(1)+mbat) = iq(lkey(1)+mbat)
 
*---- Rest of bank is integer.
      iq(lline(1)+mlfm) = 16 * 0 + 2
      iq(lline(1)+mlhd) = mlfree
 
      end
