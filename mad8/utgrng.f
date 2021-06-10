      subroutine utgrng(lrng, lseq, ipos1, ipos2, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Find range limits within the working beam line.                    *
* Input:                                                               *
*   LRNG(1)   (pointer) Range reference bank.                          *
*   LSEQ(1)   (pointer) Beam line sequence bank.                       *
* Output:                                                              *
*   IPOS1     (integer) Begin of range.                                *
*   IPOS2     (integer) End of range.                                  *
*   EFLAG     (logical) Error flag.                                    *
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
      integer ipos1,ipos2
      integer           lrng(*), lseq(*)
      logical           eflag
 
      eflag = .false.
      ipos1 = 0
      ipos2 = 0
 
*---- Find start position.
      call utgpos(lrng, lseq, 0, ipos1, eflag)
      if (.not. eflag) then
 
*---- Find end position.
        call utgpos(lrng, lseq, 3, ipos2, eflag)
        if (.not. eflag) then
 
*---- Check range for proper ordering.
          if (ipos1 .gt. ipos2) then
            call aafail('UTGRNG', 1, 'Begin and end of range inverted.')
            eflag = .true.
          endif
        endif
      endif
 
      end
