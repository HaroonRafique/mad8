      subroutine plgetn (iflag, svar, it, ipflg, sovar, sname)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Finds variable, dependent variables, axis and curve annotations    *
* Input:                                                               *
*   IFLAG    (integer)  0 for dependent variables and process flag,    *
*                       1 for axis, 2 for curve, 3 for trunc. name,    *
*                       4 to print the axis names on IQLOG             *
*   SVAR        (char)  variable to be looked up.                      *
*   IT          (int)   table number (see PLGTBS).                     *
* Output:                                                              *
*   IPFLG(1) (integer)  process flag: 0 as is, 1 take root, else call  *
*                       function PLPVAL                                *
*   IPFLG(2) (integer)  interpol. flag: 0 spline, else call            *
*                       function PLINTP                                *
*   SOVAR       (char)  array of (up to MXDEP) dependent variables     *
*   SNAME       (char)  requested annotation                           *
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
      integer maux,maxitp,maxppt,mdsv,mint,mksmax,mntmax,mnvar,mpanno,
     +mpascl,mpbars,mpbtit,mpcolr,mpfelm,mpfont,mpfram,mplscl,mplscw,
     +mpmax,mpmin,mpmxvr,mpname,mpparn,mppcrn,mpsclf,mpspli,
     +mpsscl,mpstyl,mpsymb,mpsymf,mptscl,mpttit,mpvaxr,mpxsiz,mpysiz,
     +mqadd,mtbv,mtitl,musrv,mxdep,mxipar,mxlabl,mxqbnk,mxqcnd
 
      real              pflmax
 
      parameter         (mpparn = 11, mppcrn = 170)
      parameter         (mpmxvr = 5,  mxipar = 8, mtitl  = 128)
      parameter         (mxlabl = 40, pflmax = 1.e20)
      parameter         (mtbv = 6, mdsv = 3, musrv = 3)
      parameter         (maxppt = 20000, mnvar = 74, mxdep = 2)
      parameter         (mint = 10, maux = mint + 1, maxitp = 5000)
      parameter         (mxqcnd = 10, mxqbnk = 1000, mqadd = 100000)
      parameter         (mntmax = 20, mksmax = 10)
 
      parameter         (mpfont = 1, mpxsiz = 3, mpysiz = 4)
      parameter         (mplscl = 6, mptscl = 8, mpascl = 5)
      parameter         (mplscw = 2, mpsscl = 7, mpfelm = 9)
      parameter         (mpfram = 2, mpmin  = 1, mpmax  = 2)
      parameter         (mpsclf = 3, mpvaxr = 4, mpname = 5)
      parameter         (mpstyl = 1, mpspli = 2, mpbars = 3)
      parameter         (mpsymf = 4, mpcolr = 5, mpsymb = 6)
      parameter         (mpanno = 7)
      parameter         (mpttit = mpname + mtitl / mcwrd)
      parameter         (mpbtit = mpttit + mtitl / mcwrd)
 
*--- preceding parameters: see LPMAIN description (routine PLPLOT)
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer i,iflag,index,iref,it,j,k1,k1f,k2,k2f
 
      integer           ipflg(2)
 
      character         svar*(mcnam), sovar(*)*(mcnam),
     +                     sname * (mxlabl)
      character         svlabl(mnvar)*(mxlabl)
      character         svanno(mnvar)*(mxlabl)
      character         svname(mnvar)*(mcnam)
*--- strings:
*   SVLABL   plot prescriptions for variables on axis labels
*   SVANNO   plot prescriptions for variables in annotations
*   SVNAME   names of variables known to the program
 
      integer              iproc(mnvar,3), intpo(mnvar)
      integer              ivdep(mnvar,mxdep,3)
 
      data (svname(j), j = 1, 32) /
     +'S', 'SIZE', 'DELTAP',
     +'QS', 'X', 'Y', 'XSIZE', 'YSIZE',
     +'DT', 'XN', 'YN', 'PXN', 'PYN',
     +'GAMMATR', 'XRMS', 'YRMS',
     +'XMAX', 'YMAX', 'BXMAX',
     +'BYMAX', 'DXMAX', 'DYMAX',
     +'TN', 'T', 'TURNS', 'PARTICLE', 'ALFA',
     +'PTN', 'WT', 'PHIT',
     +'RBXMAX',
     +'RBYMAX' /
      data (svname(j), j = 33, mnvar) /
     +'BETX', 'RBETX',
     +'ALFX', 'MUX', 'DX',
     +'DPX', 'QX', 'PX', 'WX',
     +'PHIX', 'DMUX',
     +'DDX', 'DDPX', 'IWX',
     +'XIX',
     +'BETY', 'RBETY',
     +'ALFY', 'MUY', 'DY',
     +'DPY', 'QY', 'PY', 'WY',
     +'PHIY', 'DMUY',
     +'DDY', 'DDPY', 'IWY',
     +'XIY', 'XNS', 'PXNS', 'WXS',
     +'YNS', 'PYNS', 'WYS',
     +'ENERGY', 'SPINTUNE',
     +'POLTOTAL', 'POLDIFFX', 'POLDIFFY', 'PT' /
 
      data (svlabl(j), j = 1, 32) /
     +'s (m)', 'n<G>s<G> (mm)', '<G>d<G><?>E<?>/p<?>0<?>c',
     +'Q<?>s<?>', 'x (m)', 'y (m)', 'n<G>s<G> (mm)', 'n<G>s<G> (mm)',
     +'ct (m)', 'x<?>n<?>', 'y<?>n<?>', 'p<?>xn<?>', 'p<?>yn<?>',
     +'<G>g<G><?>tr<?>', 'X<?>rms<?> (m)', 'Y<?>rms<?> (m)',
     +'X<?>max<?> (m)', 'Y<?>max<?> (m)', '<G>b<G><?>x_max<?> (m)',
     +'<G>b<G><?>y_max<?> (m)', 'D<?>x_max<?> (m)', 'D<?>y_max<?> (m)',
     +'t<?>n<?>', 'ct (m)', 'turns', 'particle', '<G>a<G>',
     +'p<?>t_n<?>', 'W<?>t<?>', '<G>F<G><?>t<?> (rad/2<G>p<G>)',
     +'<G>b<G><?>x_max<?><!>1/2<!> (m<!>1/2<!>)',
     +'<G>b<G><?>y_max<?><!>1/2<!> (m<!>1/2<!>)' /
      data (svlabl(j), j = 33, mnvar) /
     +'<G>b<G><?>x<?> (m)', '<G>b<G><?>x<?><!>1/2<!> (m<!>1/2<!>)',
     +'<G>a<G><?>x<?>', '<G>m<G><?>x<?> (rad/2<G>p<G>)', 'D<?>x<?> (m)',
     +'D<?>px<?>', 'Q<?>x<?>', 'p<?>x<?>/p<?>0<?>', 'W<?>x<?>',
     +'<G>F<G><?>x<?> (rad/2<G>p<G>)', 'd<G>m<G><?>x<?>/d<G>d<G>',
     +'dD<?>x<?>/d<G>d<G> (m)', 'dD<?>px<?>/d<G>d<G>', 'W<?>x<?> (m)',
     +'XI<?>x<?>',
     +'<G>b<G><?>y<?> (m)', '<G>b<G><?>y<?><!>1/2<!> (m<!>1/2<!>)',
     +'<G>a<G><?>y<?>', '<G>m<G><?>y<?> (rad/2<G>p<G>)', 'D<?>y<?> (m)',
     +'D<?>py<?>', 'Q<?>y<?>', 'p<?>y<?>/p<?>0<?>', 'W<?>y<?>',
     +'<G>F<G><?>y<?> (rad/2<G>p<G>)', 'd<G>m<G><?>y<?>/d<G>d<G>',
     +'dD<?>y<?>/d<G>d<D> (m)', 'dD<?>py<?>/d<G>d<G>', 'W<?>y<?> (m)',
     +'XI<?>y<?>', 'x<?>ns<?>', 'p<?>x_ns<?>', 'W<?>xs<?>',
     + 'y<?>ns<?>', 'p<?>y_ns<?>', 'W<?>ys<?>',
     + 'E[GeV]', 'spintune',
     + 'polarization','polarization','polarization','p<?>t<?>' /
 
      data (svanno(j), j = 1, 32) /
     +'s', 'n<G>s<G>', '<G>d<G>',
     +'Q<?>s<?>', 'x', 'y', 'n<G>s<G><?>x<?>', 'n<G>s<G><?>y<?>',
     +'ct', 'x<?>n<?>', 'y<?>n<?>', 'p<?>xn<?>', 'p<?>yn<?>',
     +'<G>g<G><?>tr<?>', 'X<?>rms<?>', 'Y<?>rms<?>',
     +'X<?>max<?>', 'Y<?>max<?>', '<G>b<G><?>x_max<?>',
     +'<G>b<G><?>y_max<?>', 'D<?>x_max<?>', 'D<?>y_max<?>',
     +'t<?>n<?>', 't', 'turns', 'particle', '<G>a<G>',
     +'p<?>t_n<?>', 'W<?>t<?>', '<G>F<G><?>t<?>',
     +'<G>b<G><?>x_max<?><!>1/2<!>',
     +'<G>b<G><?>y_max<?><!>1/2<!>' /
      data (svanno(j), j = 33, mnvar) /
     +'<G>b<G><?>x<?>', '<G>b<G><?>x<?><!>1/2<!>',
     +'<G>a<G><?>x<?>', '<G>m<G><?>x<?>', 'D<?>x<?>',
     +'D<?>px<?>', 'Q<?>x<?>', 'p<?>x<?>', 'W<?>x<?>',
     +'<G>F<G><?>x<?>', '<G>m<G><?>x<?>''',
     +'D<?>x<?>''', 'D<?>px<?>''', 'W<?>x<?>',
     +'XI<?>x<?>',
     +'<G>b<G><?>y<?>', '<G>b<G><?>y<?><!>1/2<!>',
     +'<G>a<G><?>y<?>', '<G>m<G><?>y<?>', 'D<?>y<?>',
     +'D<?>py<?>', 'Q<?>y<?>', 'p<?>y<?>', 'W<?>y<?>',
     +'<G>F<G><?>y<?>', '<G>m<G><?>y<?>''',
     +'D<?>y<?>''', 'D<?>py<?>''', 'W<?>y<?>',
     +'XI<?>y<?>', 'x<?>ns<?>', 'p<?>x_ns<?>', 'W<?>xs<?>',
     + 'y<?>ns<?>', 'p<?>y_ns<?>', 'W<?>ys<?>',
     + ' ', ' ',
     + 'p<?>tot<?>', 'p<?>diff_x<?>', 'p<?>diff_y<?>', 'p<?>t<?>'/
 
      data (iproc(j,1), j = 1, 32) /
     +0, 0, 0,
     +0, 0, 0, 0, 0,
     +0, 2, 3, 4, 5,
     +0, 0, 0,
     +0, 0, 0,
     +0, 0, 0,
     +6, 0, 0, 0, 0,
     +7, 8, 9,
     +1,
     +1 /
 
      data (iproc(j,1), j = 33, mnvar) /
     +0, 1,
     +0, 0, 0,
     +0, 0, 0, 0,
     +0, 0,
     +0, 0, 0,
     +0,
     +0, 1,
     +0, 0, 0,
     +0, 0, 0, 0,
     +0, 0,
     +0, 0, 0,
     +0, 14, 15, 16,
     +17, 18, 19,
     +0, 0,
     +0, 0, 0, 0 /
 
      data (iproc(j,2), j = 1, 32) /
     +0, 0, 0,
     +0, 0, 0, 0, 0,
     +0, 2, 3, 4, 5,
     +0, 0, 0,
     +0, 0, 0,
     +0, 0, 0,
     +6, 0, 0, 0, 0,
     +7, 8, 9,
     +1,
     +1 /
 
      data (iproc(j,2), j = 33, mnvar) /
     +0, 1,
     +0, 0, 0,
     +0, 0, 0, 10,
     +11, 0,
     +0, 0, 0,
     +0,
     +0, 1,
     +0, 0, 0,
     +0, 0, 0, 12,
     +13, 0,
     +0, 0, 0,
     +0, 14, 15, 16,
     +17, 18, 19,
     +0, 0,
     +0, 0, 0, 0 /
 
      data (iproc(j,3), j = 1, 32) /
     +0, 0, 0,
     +0, 0, 0, 0, 0,
     +0, 2, 3, 4, 5,
     +0, 0, 0,
     +0, 0, 0,
     +0, 0, 0,
     +6, 0, 0, 0, 0,
     +7, 8, 9,
     +1,
     +1 /
 
      data (iproc(j,3), j = 33, mnvar) /
     +0, 1,
     +0, 0, 0,
     +0, 0, 0, 10,
     +11, 0,
     +0, 0, 0,
     +0,
     +0, 1,
     +0, 0, 0,
     +0, 0, 0, 12,
     +13, 0,
     +0, 0, 0,
     +0, 14, 15, 16,
     +17, 18, 19,
     +0, 0,
     +0, 0, 0, 0 /
 
      data (intpo(j), j = 1, 32) / 32 * 0 /
*--- in INTPO, n+100 means: take SQRT of var. n
      data (intpo(j), j = 33, mnvar) /
     +1, 101,
     +2, 3, 4,
     +5, 0, 0, 0,
     +0, 0,
     +0, 0, 0,
     +0,
     +6, 106,
     +7, 8, 9,
     +10, 0, 0, 0,
     +0, 0,
     +0, 0, 0,
     +0, 0, 0, 0,
     +0, 0, 0,
     +0, 0,
     +0, 0, 0, 0 /
 
      data (ivdep(j,1,1), j = 1, 32) /
     +1, 2, 3,
     +4, 5, 6, 7, 8,
     +9, 5, 6, 5, 6,
     +14, 15, 16,
     +17, 18, 19,
     +20, 21, 22,
     +24, 24, 25, 26, 27,
     +3, 3, 3,
     +19,
     +20 /
      data (ivdep(j,2,1), j = 1, 32) /
     +0, 0, 0,
     +0, 0, 0, 0, 0,
     +0, 0, 0, 40, 55,
     +0, 0, 0,
     +0, 0, 0,
     +0, 0, 0,
     +0, 0, 0, 0, 0,
     +0, 24, 24,
     +0,
     +0 /
      data (ivdep(j,1,1), j = 33, mnvar) /
     +33, 33,
     +35, 36, 37,
     +38, 39, 40, 41,
     +42, 43,
     +44, 45, 46,
     +47,
     +48, 48,
     +50, 51, 52,
     +53, 54, 55, 56,
     +57, 58,
     +59, 60, 61,
     +62, 5, 5, 5,
     +6, 6, 6,
     +69, 70,
     +71, 72, 73, 74 /
      data (ivdep(j,2,1), j = 33, mnvar) /
     +0, 0,
     +0, 0, 0,
     +0, 0, 0, 0,
     +0, 0,
     +0, 0, 0,
     +0,
     +0, 0,
     +0, 0, 0,
     +0, 0, 0, 0,
     +0, 0,
     +0, 0, 0,
     +0, 0, 40, 40,
     +0, 55, 55,
     +0, 0,
     +0, 0, 0, 0 /
 
      data (ivdep(j,1,2), j = 1, 32) /
     +1, 2, 3,
     +4, 5, 6, 7, 8,
     +9, 5, 6, 5, 6,
     +14, 15, 16,
     +17, 18, 19,
     +20, 21, 22,
     +24, 24, 25, 26, 27,
     +3, 3, 3,
     +19,
     +20 /
      data (ivdep(j,2,2), j = 1, 32) /
     +0, 0, 0,
     +0, 0, 0, 0, 0,
     +0, 0, 0, 40, 55,
     +0, 0, 0,
     +0, 0, 0,
     +0, 0, 0,
     +0, 0, 0, 0, 0,
     +0, 24, 24,
     +0,
     +0 /
      data (ivdep(j,1,2), j = 33, mnvar) /
     +33, 33,
     +35, 36, 37,
     +38, 39, 40, 5,
     +5, 43,
     +44, 45, 46,
     +47,
     +48, 48,
     +50, 51, 52,
     +53, 54, 55, 6,
     +6, 58,
     +59, 60, 61,
     +62, 5, 5, 5,
     +6, 6, 6,
     +69, 70,
     +71, 72, 73, 74 /
      data (ivdep(j,2,2), j = 33, mnvar) /
     +0, 0,
     +0, 0, 0,
     +0, 0, 0, 40,
     +40, 0,
     +0, 0, 0,
     +0,
     +0, 0,
     +0, 0, 0,
     +0, 0, 0, 55,
     +55, 0,
     +0, 0, 0,
     +0, 0, 40, 40,
     +0, 55, 55,
     +0, 0,
     +0, 0, 0, 0 /
 
      data (ivdep(j,1,3), j = 1, 32) /
     +1, 2, 3,
     +4, 5, 6, 7, 8,
     +9, 5, 6, 5, 6,
     +14, 15, 16,
     +17, 18, 19,
     +20, 21, 22,
     +24, 24, 25, 26, 27,
     +3, 3, 3,
     +19,
     +20 /
      data (ivdep(j,2,3), j = 1, 32) /
     +0, 0, 0,
     +0, 0, 0, 0, 0,
     +0, 0, 0, 40, 55,
     +0, 0, 0,
     +0, 0, 0,
     +0, 0, 0,
     +0, 0, 0, 0, 0,
     +0, 24, 24,
     +0,
     +0 /
      data (ivdep(j,1,3), j = 33, mnvar) /
     +33, 33,
     +35, 36, 37,
     +38, 39, 40, 5,
     +5, 43,
     +44, 45, 46,
     +47,
     +48, 48,
     +50, 51, 52,
     +53, 54, 55, 6,
     +6, 58,
     +59, 60, 61,
     +62, 5, 5, 5,
     +6, 6, 6,
     +69, 70,
     +71, 72, 73, 74 /
      data (ivdep(j,2,3), j = 33, mnvar) /
     +0, 0,
     +0, 0, 0,
     +0, 0, 0, 40,
     +40, 0,
     +0, 0, 0,
     +0,
     +0, 0,
     +0, 0, 0,
     +0, 0, 0, 55,
     +55, 0,
     +0, 0, 0,
     +0, 0, 40, 40,
     +0, 55, 55,
     +0, 0,
     +0, 0, 0, 0 /
 
      if (it .le. 0 .or. it .gt. 3)  then
        sovar(1) = svar
        sovar(2) = ' '
        sname    = svar
        ipflg(1) = 0
        ipflg(2) = 0
        goto 999
      endif
      sovar(1) = ' '
      if (iflag .eq. 4)  then
*--- printing on ECHO file requested for all names
        write(iqlog, '(/'' Number of PLOT variables ='',I5/)') mnvar
        write(iqlog, '(4X,4A18)')  (svname(i), i = 1, mnvar)
        write(iqlog, *) ' '
        goto 999
      endif
      sname = svar
*--- search in list of known variables
      do 1  iref = 1, mnvar
        if (svar .eq. svname(iref))  goto 9
    1 continue
      call gxpnbl(svar, k1, k2)
      do 2  iref = 1, mnvar
        call gxpnbl(svname(iref), k1f, k2f)
        if (k2 + 1 .eq. k2f)  then
          if (index('XY', svname(iref)(k2f:k2f)) .ne. 0)  then
            if (svar(:k2) .eq. svname(iref)(:k2))  goto 9
          endif
        endif
    2 continue
      goto 999
    9 continue
      if (iflag .eq. 0)  then
        sname = svname(iref)
        ipflg(1) = iproc(iref,it)
        ipflg(2) = intpo(iref)
        do 10  j = 1, mxdep
          if (ivdep(iref,j,it) .eq. 0)  then
            sovar(j) = ' '
          else
            sovar(j) = svname(ivdep(iref,j,it))
          endif
   10   continue
      elseif (iflag .eq. 1) then
        sname = svlabl(iref)
        if (svar .ne. svname(iref))  then
*--- incomplete match
*    replace x or y in name by blank
          call gxpnbl(sname, k1, k2)
          do 20  i = 2, k2
            if (index('XYxy', sname(i:i)) .ne. 0)  then
              sname(i:i) = ' '
            endif
   20     continue
        endif
      elseif (iflag .eq. 2) then
        sname = svanno(iref)
      elseif (iflag .eq. 3) then
        if (svar .eq. svname(iref))  then
          sname = svname(iref)
        else
          sname = svname(iref)(:k2)
        endif
      else
        sname = svar
      endif
  999 end
