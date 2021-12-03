!
      subroutine errmsgcx(roumri,roucpx,rtncpx,errmsg)
!
!  Routine to decode the long error message returned by 'CPXgeterrorstring',
!   print the message and return control to the calling routine which may
!   either stop the program or continue depending upon location of problem.
!
!  Input Arguments:
!            roumri : character string containing the name of the MRI routine
!                      in which the error occurred
!            roucpx : character string containing the name of the CPLEX
!                      that resulted in a call to 'CPXgeterrorstring'
!            rtncpx : integer value returned by the CPLEX function 'roucpx'
!            errmsg : character string error message returned by
!                      'CPXgeterrorstring'
!
!  Output Arugments:  None
!           
!  Written By:  Ike Patterson                           On:  1 Jun 98
!
!  Last Modified On:   2 Jun 98
!
!  Incorporate symbolic type module and nonprintable ASCII characters module
!
      use symtypes
!
!!      use spcascii, only: NULLC, OMIT
!
!  Allow no implicit declarations
!
      implicit none
!
!  Define parameters 1) adding the space character to the nonprintable ASCII
!   characters and 2) two character string consisting of the NULL character
!   and a space and 3) end of sentence punctuation
!
!!      character(*), parameter :: OMITSP = OMIT//' '
!
!!      character(2), parameter :: NULLSP = NULLC//' '
!
      character(3), parameter :: EOS = '.!?'
!
!  Define parameter for the maximum number of characters allowed per line
!   less than or equal to 80
!
      integer(IL), parameter :: NCPL = 76
!
!  Define parameter for integer string length
!
      integer(IL), parameter :: INTLEN = 14
!
!  Interface for 'inttostr' function
!
!c      INTERFACE
!c      function inttostr(intg,comi,beg,como)
!c      use symtypes
!c      character() :: inttostr
!c      integer(IL), intent(in) :: intg, comi
!c      integer(IL), intent(out) :: beg, como
!c      end function inttostr
!c      END INTERFACE
!
!  Declare input arguments
!
      character(*), intent(in) :: roumri, roucpx, errmsg
!
      integer(IL), intent(in) :: rtncpx
!
!  Declare local variables
!
      character(len(errmsg) + 1) :: locmsg
      character(INTLEN) :: intstr, inttostr
      character(NCPL) :: curent
!
      integer(IL) :: iend, ierr, istr, jcur, kcur, loclen, longest,     &
     &               number, scratch, tab
!
      logical(LD) :: isopen
!
!  Write first portion of message
!
      iend = len_trim(adjustl(roumri))
      istr = 37 + iend
      tab = (80 - istr)/2
      if (2*tab < (80 - istr)) tab = tab + 1
      tab = tab + 1
      write (*,100) trim(adjustl(roumri))
!
      iend = len_trim(adjustl(roucpx))
      istr = 35 + iend
      tab = (80 - istr)/2
      if (2*tab < (80 - istr)) tab = tab + 1
      tab = tab + 1
      write (*,120) trim(adjustl(roucpx))
!
      intstr = inttostr(rtncpx,1,jcur,kcur)
      iend = INTLEN - jcur + 1
      istr = 46 + iend
      tab = (80 - istr)/2
      if (2*tab < (80 - istr)) tab = tab + 1
      tab = tab + 1
      write (*,140) intstr(jcur:INTLEN)
!
!  Find first and last nonblank printable characters
!
!!      istr = verify(errmsg,OMITSP)
!!      iend = verify(errmsg,OMITSP,.true.)
!
!  Write message depending upon content of error message string
!
      if (istr == 0 .or. istr == iend) then
!
!  Empty or meaningless error message
!
         write (*,200)
!
      else
!
!  Put printable portion of error string into local string and terminate
!   with a NULL character
!
         loclen = iend - istr + 2
!!         locmsg = errmsg(istr:iend)//NULLC
!
!  Replace all interior nonprintable characters with a space
!
         iend = 0
         do
            istr = iend + 1
            if (istr == loclen) exit
!!            jcur = scan(locmsg(istr:loclen),OMIT)
            iend = istr + jcur - 1
            if (iend == loclen) exit
            locmsg(iend:iend) = ' '
         end do
!
!  Replace all interior substrings consisting of multiple (more than 1)
!   successive spaces with a single space exit after sentence ending
!   punctuation ('.', '!', or '?') where two spaces will be kept if they
!   are already present
!
         iend = 0
         do
            istr = iend + 1
            if (istr == loclen) exit
!!            jcur = scan(locmsg(istr:loclen),NULLSP)
            jcur = istr + jcur - 1
            if (jcur == loclen) exit
            kcur = scan(locmsg((jcur - 1):(jcur - 1)),EOS)
            if (kcur == 1 .and. locmsg((jcur + 1):(jcur + 1)) == ' ')   &
     &         jcur = jcur + 1
            kcur = verify(locmsg((jcur + 1):loclen),' ')
            kcur = jcur + kcur
            if (kcur == loclen) then
               loclen = jcur
!!               locmsg(1:loclen) = locmsg(1:jcur)//NULLC
               exit
            end if
            if (jcur == (kcur - 1)) then
               iend = kcur
            else
               iend = jcur + loclen - kcur + 1
               locmsg(1:iend) = locmsg(1:jcur)//locmsg(kcur:loclen)
               loclen = iend
               iend = jcur + 1
            end if
         end do
!
!  Open scratch file if possible
!
         scratch = 6
         do
            scratch = scratch + 1
            inquire (unit = scratch, iostat = ierr, opened = isopen)
            if (isopen) then
               if (ierr /= 0) exit
            else
               exit
            end if
         end do
         if (ierr == 0) open (scratch, iostat = ierr,                   &
     &                        status = 'scratch')
!
!  If scratch file opened, use it to center message as much as possible,
!   otherwise, make as neat as possible printing one line at a time with
!   no info on succeeding lines
!
         iend = 0
         loclen = loclen - 1
!
         if (ierr == 0) then
!
            longest = 0
            number = 0
            ierr = loclen + 1
            do
               istr = iend + 1
               if ((ierr - istr) <= NCPL) then
                  iend = loclen
               else
                  iend = iend + NCPL
                  if (locmsg((iend + 1):(iend + 1)) /= ' ') then
                     jcur = scan(locmsg(istr:iend),' ',.true.)
                     if (jcur > 0) iend = istr + jcur - 2
                     if (locmsg(iend:iend) == ' ') iend = iend - 1
                  else if (locmsg(iend:iend) == ' ') then
                     iend = iend - 1
                  end if
               end if
               kcur = iend - istr + 1
               write (scratch,300) kcur, locmsg(istr:iend)
               number = number + 1
               longest = max0(longest,kcur)
               if (iend == loclen) exit
               iend = iend + 1
               if (locmsg((iend + 1):(iend + 1)) == ' ') iend = iend + 1
            end do
            tab = (80 - longest)/2
            if (2*tab < (80 - longest)) tab = tab + 1
            tab = tab + 1
            rewind (scratch)
            do istr = 1, number
               read(scratch,320) kcur, curent
               write (*,400) curent(1:kcur)
            end do
            close (scratch)
!
         else
!
            ierr = loclen + 1
            tab = (80 - NCPL)/2
            if (2*tab < (80 - NCPL)) tab = tab + 1
            tab = tab + 1
            do
               istr = iend + 1
               if ((ierr - istr) <= NCPL) then
                  iend = loclen
               else
                  iend = iend + NCPL
                  if (locmsg((iend + 1):(iend + 1)) /= ' ') then
                     jcur = scan(locmsg(istr:iend),' ',.true.)
                     if (jcur > 0) iend = istr + jcur - 2
                     if (locmsg(iend:iend) == ' ') iend = iend - 1
                  else if (locmsg(iend:iend) == ' ') then
                     iend = iend - 1
                  end if
               end if
               kcur = iend - istr + 1
               write (*,400) locmsg(istr:iend)
               if (iend == loclen) exit
               iend = iend + 1
               if (locmsg((iend + 1):(iend + 1)) == ' ') iend = iend + 1
            end do
!
         end if
!
      end if
!
!  Write final portion of message
!
      iend = len_trim(adjustl(roumri))
      istr = 36 + iend
      tab = (80 - istr)/2
      if (2*tab < (80 - istr)) tab = tab + 1
      tab = tab + 1
      write (*,500) trim(adjustl(roumri))
!
!  Format specifications
!
  100 format (//,80('*'),//,t<tab>,'Error Encountered In MRI Routine:', &
     & '  ''',a<iend>,'''',/)
!
  120 format (t<tab>,'During A Call To CPLEX Routine:  ''',a<iend>,     &
     & '''',/)
!
  140 format (t<tab>,'Which Returned An Error Indication Value Of:  ',  &
     & a<iend>,//,t3,'A Subsequent Call To ''CPXgeterrorstring'' ',     &
     & 'Resulted In The Following Message -',//)
!
  200 format (t15,'###  Empty Or Meaningless Error Message String  ###')
!
  300 format (t2,i3,2x,a<kcur>)
!
  320 format (t2,i3,2x,a<NCPL>)
!
  400 format (t<tab>,a<kcur>)
!
  500 format (//,t9,'See Appendix C ''CPLEX Error Messages'' Of The ',  &
     & 'CPLEX Documentation',//,t16,'''Using The CPLEX Callable ',      &
     & 'Library'' For More Detail',//,t<tab>,'Control Returned To MRI ',&
     & 'Routine:  ''',a<iend>,'''',//,80('*'),//)
!
      end subroutine errmsgcx