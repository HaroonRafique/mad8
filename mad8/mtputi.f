      subroutine mtputi(nx, x)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Set external parameter values from internal ones.                  *
* Input:                                                               *
*   X(NX)     (real)    Internal parameter values.                     *
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
      integer iref,ivar,nx
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
 
      lvar = lmvar
      ivar = 0
   10 if (lvar .ne. 0) then
        lref = lq(lvar-1)
        call ucopy(q(lvar+mmdata), vstep, 3*mwflt)
        ivar = ivar + 1
        if (iq(lvar+2) .eq. 0) then
          vval = x(ivar)
        else if (iq(lvar+2) .eq. 1) then
          vval = vmin + x(ivar)**2
        else if (iq(lvar+2) .eq. 2) then
          vval = vmax - x(ivar)**2
        else
          vval = ((vmin + vmax) + (vmax - vmin) * sin(x(ivar))) / 2.0
        endif
        lref = lq(lvar-1)
        iref = iq(lvar+mmbias)
        call utpflt(lref, iref, iref, vval)
        lvar = lq(lvar)
        go to 10
      endif
 
*---- Update dependent variables.
      call exupdt
 
*---- Drop precomputed maps which became obsolete.
      call mtpmod
 
      end
