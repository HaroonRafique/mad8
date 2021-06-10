      subroutine mtprnt(nx, x)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print minimum information.                                         *
* Input:                                                               *
*   NX        (integer) Number of variables.                           *
*   X(NX)     (real)    Variable vector.                               *
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
      integer jref,nx
      double precision vval,x
      dimension         x(nx)
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
      integer mcf1,mcf2,mcsiz,mctyp,mcval
 
*---- Bias for command attribute groups.
      parameter         (mcf1 = 1, mctyp = 2, mcf2 = 3, mcval = 4,
     +                   mcsiz = mwnam + 3)
      integer mmbias,mmbnam,mmcode,mmdata,mmf1,mmf2,mmf3,mmold,mmsiz,
     +mmvnam
 
*---- Bias for variable parameters group.
      parameter         (mmf1   = 1, mmcode = 2, mmbias = 3,
     +                   mmf2   = 4, mmold = 5, mmdata = mmold+mwflt,
     +                   mmf3   = mmdata+4*mwflt, mmbnam = mmf3+1,
     +                   mmvnam = mmbnam+mwnam, mmsiz = mmf3+2*mwnam)
      integer lcon,lmcon,lmtbet,lmtlin,lmtseq,lmtsub,lmvar,lptr,lref,
     +lsmat,lvar,lbeta0
 
*---- Link area for matching.
      common /mtlink/   lsmat, lmcon, lmvar,
     +                  lmtlin, lmtseq, lmtbet, lbeta0(2), lmtsub,
     +                  lcon, lref, lvar, lptr
      save              /mtlink/
      integer ivcode
      double precision vmax,vmin,vold,vstep
 
*---- Working area for a single matching variable.
      common /mtcvar/   vold, vstep, vmin, vmax, ivcode
      save   /mtcvar/
      integer icall,icovar,ifirst,ilevel,imode,istrat,ncon,nfcn,nfcnmx,
     +nvar
      double precision edm,fmin,tol,up
 
*---- Communication area for routines derived from MINUIT.
      common /minchr/   crout, cstat
      common /mindbl/   edm, fmin, tol, up
      common /minint/   icall, icovar, ifirst, imode, ilevel, istrat,
     +                  ncon, nvar, nfcn, nfcnmx
      common /minflt/   time1, time2
      save              /minchr/, /mindbl/, /minint/, /minflt/
      character         crout*8, cstat*16
      real              time1, time2
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      character*(mcnam) bnknam, atrnam
      real              time
 
      call timex(time)
      write (iqpr2, 910) crout, time, nfcn, cstat, fmin, edm
 
      lvar = lmvar
   10 if (lvar .ne. 0) then
        lref = lq(lvar-1)
        jref = mbat + (iq(lvar+mmbias) - 1) * mcsiz
        call ucopy(q(lref+jref+mcval), vval, mwflt)
        call ucopy(q(lvar+mmdata), vstep, 3*mwflt)
        call uhtoc(q(lvar+mmbnam), mcwrd, bnknam, mcnam)
        call uhtoc(q(lvar+mmvnam), mcwrd, atrnam, mcnam)
        write (iqpr2, 920) bnknam, atrnam, vval, vstep, vmin, vmax
        lvar = lq(lvar)
        go to 10
      endif
 
  910 format(' '/' Command: ',a,t31,'Time: ',f12.3,t61,'Calls: ',i8,
     +       t91,'Status: ',a/' Penalty function: ',t35,1p,e14.6,
     +       t61,'Estimated distance to minimum: ',e14.6/
     +       ' '/' Element',t21,'attribute',t45,'value',t61,'step',
     +       t77,'lower',t93,'upper')
  920 format(' ',a,t21,a,t41,1p,4e16.6)
 
      end
