      subroutine dcattr(itype, lbank, ifrst, ilast, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode an attribute and store it in a data bank.                   *
*   Optionally, the datum is copied according to NREPT.                *
* Input:                                                               *
*   ITYPE    (integer)  Data type code.                                *
*   LBANK(1) (pointer)  Data bank pointer.                             *
*   IFRST    (integer)  First attribute number.                        *
*   ILAST    (integer)  Highest attribute number allowed.              *
* Output:                                                              *
*   EFLAG    (logical)  Error flag.                                    *
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
      integer idata,ieval,iexpr,ifrst,ilast,ilink,iseen,itype,ival,
     +jdata,jlink,jrept,nint,nrept
      double precision rval,rval1,rval2,rval3
      integer           lbank(*)
      logical           eflag
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
      integer ldcatt,ldcbnk,ldcfrm,ldckey,ldclin
 
*---- Local links for decoder.
      common /dclink/   ldcatt, ldcbnk, ldcfrm, ldckey, ldclin
      save              /dclink/
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
 
      logical           lval
 
*---- Decode repeat count.
      ldcbnk = lbank(1)
      eflag = .false.
      call dcrept(nrept)
      if (ifrst + nrept - 1 .gt. ilast) then
        call rdwarn('DCATTR', 1,
     +  'Repeat count too large --- reset to dimension limit.')
        nrept = ilast - ifrst + 1
      endif
      ilink = ifrst
      idata = mbat + mcsiz * (ilink - 1)
 
*---- Branch by data type to read further.
*            NAM  INT  FLT  DEF  LOG  STR  LIN  RNG  CST  VAR
      go to (100, 200, 300, 300, 400, 500, 600, 700, 800, 900), itype
      go to 9999
 
*---- 1.  Name.
  100 continue
        call dcname(ilink, idata, eflag)
      go to 1000
 
*---- 2.  Integer.
  200 continue
        call rdint(ival, eflag)
        iseen = 10 * mtint
        if (.not. eflag) iseen = 10 * mtint + 1
        iq(ldcbnk+idata+mctyp) = iseen
        iq(ldcbnk+idata+mcval) = ival
      go to 1000
 
*---- 3, 4.  Real, deferred.
  300 continue
        ieval = itype - 1
        call exread(ieval, rval1, iexpr)
        if (token(jtok) .eq. ':') then
          if (iexpr .ne. 1) go to 350
          if (itype .eq. mtdef) then
            call rdfail('DCATTR', 1,
     +      '"start:end:step" not allowed for deferred data.')
          else
            jtok = jtok + 1
            call exread(2, rval2, iexpr)
            if (iexpr .ne. 1) go to 350
            call rdtest(':', eflag)
            if (eflag) go to 9999
            jtok = jtok + 1
            call exread(2, rval3, iexpr)
            if (iexpr .ne. 1) go to 350
            if (rval3 .eq. 0.0) then
              call rdfail('DCATTR', 1,
     +        '"start:end:step": "step" must not be zero.')
            else
              nrept = nint((rval2 - rval1) / rval3)
              if (ifrst + nrept .gt. ilast) then
                call rdfail('DCATTR', 1,
     +          '"start:end:step" causes array overflow.')
              else
                do 310 jrept = 0, nrept
                  rval = rval1 + jrept * rval3
                  call exmake(ldcbnk, ilink, idata + mcval, rval, 1)
                  iq(ldcbnk+idata+mctyp) = 10 * mtflt + iexpr
                  idata = idata + mcsiz
                  ilink = ilink + 1
  310           continue
              endif
            endif
          endif
        else
          do 320 jrept = 1, nrept
            call exmake(ldcbnk, ilink, idata + mcval, rval1, iexpr)
            iq(ldcbnk+idata+mctyp) = 10 * mtflt + iexpr
            idata = idata + mcsiz
            ilink = ilink + 1
  320     continue
        endif
        ifrst = ilink
      go to 9999
 
*---- Invalid use of colon.
  350 continue
      if (iexpr .ne. 0) then
        call rdfail('DCATTR', 1,
     +  '"start:end:step" must not contain variable expression.')
      endif
      go to 9999
 
*---- 5.  Logical.
  400 continue
        call rdlogc(lval, eflag)
        iseen = 10 * mtlog
        if (.not. eflag) iseen = 10 * mtlog + 1
        iq(ldcbnk+idata+mctyp) = iseen
        ival = 0
        if (lval) ival = 1
        iq(ldcbnk+idata+mcval) = ival
      go to 1000
 
*---- 6. String.
  500 continue
        call dcstrg(ilink, idata, eflag)
      go to 1000
 
*---- 7. Line, or other relation.
  600 continue
        call dcbeam(ilink, idata, eflag)
      go to 1000
 
*---- 8. Place, or range limits.
  700 continue
        call dcrang(ilink, idata, eflag)
      go to 1000
 
*---- 9. Constraint.
  800 continue
        call dccons(ilink, idata, eflag)
      go to 1000
 
*---- 10. Variable.
  900 continue
        call dcvref(ilink, idata, eflag)
 
*---- Store repeated values.
 1000 continue
      jlink = ilink + 1
      jdata = idata + mcsiz
      do 1010 jrept = 2, nrept
        call ucopy(q(ldcbnk+idata+1), q(ldcbnk+jdata+1), mcsiz)
        ldcatt = lq(ldcbnk-ilink)
        if (ldcatt .ne. 0) then
          call mzcopy(2, ldcatt, 2, ldcbnk, -jlink, 'Z')
        endif
        jlink = jlink + 1
        jdata = jdata + mcsiz
 1010 continue
      ifrst = jlink
 
 9999 end
