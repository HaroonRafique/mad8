      subroutine plgarw (mxb, mxc, qc, ir, ntot, ibk, ierr)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Returns Arnold web constrains in inverse Polish notation, coded    *
*                                                                      *
*--- Input                                                             *
*   MXB       (integer) max. length of output array                    *
*   MXC       (integer) max. number of constrains                      *
*   QC           (char) constrains as read                             *
*   IR        (integer) lower, upper limit, step                       *
*---Output                                                             *
*   NTOT      (integer) length occupied in output array                *
*   IBK       (integer) output array containing:                       *
*                       number of superperiods                         *
*                       number of constraints  N                       *
*                       N constraints:                                 *
*                       minimum                                        *
*                       maximum                                        *
*                       step                                           *
*                       Length L                                       *
*                       expression in polish notation of length L,     *
*                       coded as 1+, 2-, 3*, 4/, 1 KX, 2 KY, 3 KS      *
*                       + MQADD + 4, e.g. 100006 = KY                  *
*   IERR      (integer) error flag: 0 OK, else wrong expression        *
*                                                                      *
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
      integer ichtyp
 
*---- Character code translation tables.
      common /chcode/   ch2low(0:255), ch2upp(0:255)
      common /chtype/   ichtyp(0:255)
      save              /chcode/, /chtype/
      character*1       ch2low, ch2upp
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
      integer i,ic,ichar,ierr,ifirst,in,index,iv,j,kn,l,lev,lin,lstore,
     +mxb,mxc,n,nc,ncc,ntot,number
      integer ir(3,*), ibk(*)
      integer isnum(2), ienum(2), ispec(100), ioper(100)
      character * (mcstr)  qc(*), temp
      character * 4        opera
      character * 10       numeri
      character * 36       alpha
      character * 1        a
      data opera  / '+-*/' /
      data numeri / '0123456789' /
      data alpha  / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789' /
 
      ierr = 0
      ntot = 2
      ncc  = 0
      do 90 ic = 1, mxc
        n = 0
*--- remove blanks
        lin = 0
        do 10 i = 1, mcstr
          if (qc(ic)(i:i) .ne. ' ')  then
            lin = lin + 1
            temp(lin:lin) = ch2upp(ichar(qc(ic)(i:i)))
          endif
   10   continue
        if (lin .ne. 0)  then
          lin = lin + 1
          temp(lin:lin) = ' '
          ncc = ncc + 1
          ibk(ntot+1) = ir(1,ic)
          ibk(ntot+2) = ir(2,ic)
          ibk(ntot+3) = ir(3,ic)
          ntot = ntot + 4
          lstore=0
          ifirst=0
          nc=0
          number=0
          lev=1
          do 11  i=1,100
            ispec(i) = 0
            ioper(i) = 0
   11     continue
*
*--- start of decoding loop
*
          do 50 in = 1, lin
            a = temp(in:in)
*
*   check for variable or number
*
            if(index(alpha, a) .ne. 0) then
              if(number .eq. 0) then
                number = 1
                nc = nc + 1
                isnum(nc) = in
              endif
            else
*
*   operator or bracket
*
              if(number .ne. 0) then
                number = 0
                ienum(nc) = in - 1
              endif
              if(a .eq. '(') then
                lev = lev + 1
              elseif(a .eq. ')') then
                if(ispec(lev) .ne. 0) then
                  ispec(lev) = 0
                  lev = lev - 1
                endif
                lev = lev - 1
              elseif(index(opera, a) .ne. 0)  then
                if(ifirst .ne. 0 .and. nc .eq. 1 .or. nc .eq. 2) then
*
*   output number(s) and/or operators
*
                  ifirst = 1
                  do 30 j = 1 , nc
                    kn = index(numeri, temp(isnum(j):isnum(j)))
                    if (kn .ne. 0)  then
*--- number
                      iv = 0
                      do 20 l = isnum(j), ienum(j)
                        kn = index(numeri, temp(l:l))
                        if (kn .eq. 0)  then
*--- illegal - letter among digits
                          ierr = 1
                          goto 999
                        endif
                        iv = 10 * iv + kn - 1
   20                 continue
                    else
*--- must be KX, KY, or KS
                      kn =
     +                (index('KXKYKS', temp(isnum(j):ienum(j))) + 1) / 2
                      if (kn .eq. 0)  then
                        ierr = 1
                        goto 999
                      endif
                      iv = mqadd + 4 + kn
                    endif
                    n = n + 1
                    ibk(ntot+n) = iv
   30             continue
                  nc=0
                endif
*
*--- set special flag for operator sequences
*
                if((a. eq. '+' .or. a .eq. '-') .and. ispec(lev) .ne. 0)
     +          then
                  ispec(lev) = 0
                  lev = lev - 1
                endif
*
*--- output operator(s)
*
                do 40 j = lstore, lev, -1
                  if(ioper(j) .ne. 0) then
                    n = n + 1
                    ibk(ntot+n) = ioper(j)
                    ioper(j) = 0
                  endif
   40           continue
                lstore = lev
                ioper(lstore) = index(opera, a) + mqadd
                if(a .eq. '+' .or. a .eq. '-') then
                  lev = lev + 1
                  ispec(lev) = 1
                endif
              endif
            endif
   50     continue
*
*   end of constraint expression - code numbers and operators left
*
          do 70 j = 1 , nc
            kn = index(numeri, temp(isnum(j):isnum(j)))
            if (kn .ne. 0)  then
*--- number
              iv = 0
              do 60 l = isnum(j), ienum(j)
                kn = index(numeri, temp(l:l))
                if (kn .eq. 0)  then
*--- illegal - letter among digits
                  ierr = 1
                  goto 999
                endif
                iv = 10 * iv + kn - 1
   60         continue
            else
*--- must be KX, KY, or KS
              kn =
     +        (index('KXKYKS', temp(isnum(j):ienum(j))) + 1) / 2
              if (kn .eq. 0)  then
                ierr = 1
                goto 999
              endif
              iv = mqadd + 4 + kn
            endif
            n = n + 1
            ibk(ntot+n) = iv
   70     continue
          do 80 j = lstore, 1, -1
            if(ioper(j) .ne. 0) then
              n = n + 1
              ibk(ntot+n) = ioper(j)
            endif
   80     continue
          ibk(ntot) = n
          ntot = ntot + n
        endif
   90 continue
      ibk(2) = ncc
  999 end
