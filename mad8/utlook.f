      subroutine utlook(word, dict, ndict, idict)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Find WORD in dictionary DICT, accepting unique abbreviations.      *
* Input:                                                               *
*   WORD        (char)  Word to be looked up.                          *
*   DICT(NDICT) (char)  Table of allowed values.                       *
* Output:                                                              *
*   IDICT    (integer)  Position of WORD in DICT                       *
*                       (zero if word is blank or not found).          *
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
      integer idict,jdict,leng,n,ndict
      character*(*)     word
      character*(mcnam) dict(*)
 
      character*(mcnam) temp
 
*---- First try unabbreviated form.
      temp = word
      do 10 idict = 1, ndict
        if (temp .eq. dict(idict)) go to 9999
   10 continue
 
*---- WORD not found:  Try abbreviations,  N counts ambiguities.
      call utleng(word, leng)
      n = 0
      if (leng .ge. 2) then
        do 20 jdict = 1, ndict
          temp = dict(jdict)(1:leng)
          if (word .eq. temp) then
            idict = jdict
            n = n + 1
          endif
   20   continue
      endif
      if (n .ne. 1) idict = 0
 
 9999 end
