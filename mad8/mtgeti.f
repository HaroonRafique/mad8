      subroutine mtgeti(nx, x, dx)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Get internal parameter values from external ones.                  *
*   Also return steps sizes in terms of internal values.               *
*   This routine also marks all dependent elements for dropping maps.  *
* Input:                                                               *
*   X(NX)     (real)    Internal parameter values.                     *
*   DX(NX)    (real)    Internal step sizes.                           *
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
      integer idir,iref,ivar,nx
      double precision arg1,arg2,dx,eps,stplim,vval,x
      dimension         x(nx), dx(nx)
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
      integer mxals,mxcls,mxdef,mxdrp,mxknw,mxlmp,mxmod,mxord
 
*---- Expression marker bits.
      parameter         (mxdrp = 1, mxdef = 2, mxord = 3,
     +                   mxcls = 4, mxals = 5, mxlmp = 6,
     +                   mxmod = 7, mxknw = 8)
      integer laabnk,laacur,laadef,laakey,laanew,laaold,laaprc,laasrc,
     +laastk,laatar
 
*---- Local links for control module.
      common /aalink/   laabnk, laadef, laasrc, laatar, laakey, laacur,
     +                  laaprc, laastk, laanew, laaold
      save              /aalink/
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
      integer lcali,lcatt,lccls,lccmd,lccom,lcdef,lcelm,lcexp,lcfld,
     +lckey,lcseq,lcspl,lcsrc,lcvar,ldbnk,ldkey,lref1,lref2,lsali,lscom,
     +lsdir,lsfld,lsflg,lsnum,lsspl,lbeam,lconsm,ldummy
 
*---- Global reference links.
      common /refer/    lref1,
     +                  lcali, lcatt, lccls, lccmd, lccom, lcdef, lcelm,
     +                  lcexp, lcfld, lckey, lcseq, lcspl, lcsrc, lcvar,
     +                  lbeam, lconsm, ldbnk(4), ldkey(4), ldummy(10),
     +                  lsali, lscom, lsdir, lsfld, lsflg, lsnum, lsspl,
     +                  lref2
      save              /refer/
      integer liftseq, currseq
      common /seqinfi/ liftseq, currseq
      character * (mcnam) sequnam, seqnames
      common /seqinfc/ sequnam, seqnames(mttact)
 
      parameter         (eps = 1.0d-10, stplim = 2.0d-1)
 
*---- Loop over variables.
      lvar = lmvar
      ivar = 0
   10 if (lvar .ne. 0) then
        lref = lq(lvar-1)
        iref = iq(lvar+mmbias)
 
*---- Fetch internal value.
        call utgflt(lref, iref, iref, vval)
 
*---- Mark bank as dependent.
        call aamark('MTGETI', lref)
 
*---- Convert to unlimited value
        call ucopy(q(lvar+mmdata), vstep, 3*mwflt)
        ivar = ivar + 1
        if (iq(lvar+2) .eq. 0) then
          x(ivar) = vval
          dx(ivar) = abs(vstep)
        else if (iq(lvar+2) .eq. 1) then
          arg1 = max(vval - vmin, eps)
          x(ivar) = sqrt(arg1)
          dx(ivar) = max(min(abs(vstep / arg1), stplim), eps) * x(ivar)
        else if (iq(lvar+2) .eq. 2) then
          arg2 = max(vmax - vval, eps)
          x(ivar) = sqrt(arg2)
          dx(ivar) = max(min(abs(vstep / arg2), stplim), eps) * x(ivar)
        else
          arg1 = max(vval - vmin, eps)
          arg2 = max(vmax - vval, eps)
          x(ivar) = asin((arg1 - arg2) / (arg1 + arg2))
          dx(ivar) = abs(vstep) / sqrt(arg1 * arg2)
          if (dx(ivar) .gt. stplim) dx(ivar) = stplim
          if (dx(ivar) .lt. eps) dx(ivar) = eps
        endif
 
*---- Next variable.
        lvar = lq(lvar)
        go to 10
      endif
 
*---- Propagate marks on dependent elements.
      call exupdt
 
*---- Propagate marks on dependent lumps.
      call difind(ldkey, 'LUMP', idir, laakey)
      laacur = lq(laakey-1)
   20 if (laacur .ne. 0) then
        call lnpmod(laacur)
        laacur = lq(laacur)
        go to 20
      endif
 
      end
