!
      function inttostr(intg,comi,beg,como)
!
!  Character function to right align the integer 'intg' in the string
!   'inttostr' and return the first nonblank position of 'inttostr' as
!   'beg'.  If 'intg' will not fit into 'inttostr', a string of asterisks
!   ('*') are returned and 'beg' returns 0.  Commas are also added if
!   appropriate and they will fit and 'comi' is input as 1 - if 'comi' is
!   not input as 1, then no attempt is made to add commas.  On output,
!   'como' returns 1 if commas were added and 0 if no commas added.
!
!  NOTE:  See function 'sgltostr' for single precision scalar to string
!          conversion and function 'dbltostr' for double precision scalar
!          to string conversion.
!
!  Written By:  Ike Patterson                     On:  3 Jan 98
!
!  Last Modified On:   1 Jun 98
!
!  Incorporate symbolic types module
!
      use symtypes
!
!  Allow no implicit declarations
!
      implicit none
!
!  Declare output result
!
      character(*) :: inttostr
!
!  Declare input/output arguments
!
      integer(IL), intent(in) :: intg, comi
      integer(IL), intent(out) :: beg, como
!
!  Declare local variables
!
      integer(IL) :: antg, curr, last, leni, lens, n, ncom
!
!  Find length of output string 'inttostr' and initialize 'inttostr' to all
!   blanks
!
      lens = len(inttostr)
      inttostr = ' '
!
!  Initialize value of 'como' to zero
!
      como = 0
!
!  Determine number of positions required for the absolute value of 'intg'
!
      antg = iabs(intg)
      if (antg < 10) then
         leni = 1
      else
         leni = ifix(alog10(float(antg))) + 1
      end if
!
!  Return string of asterisks ('*') if 'intg' will not fit into 'inttostr'
!
      if ((intg < 0 .and. (leni + 1) > lens) .or.                       &
     &    (intg >= 0 .and. leni > lens)) then
         inttostr = repeat('*',lens)
         beg = 0
         return
      end if
!
!  Write the absolute value of 'intg' to 'inttostr'
!
      beg = lens - leni + 1
      write (inttostr(beg:lens),'(i<leni>)') antg
!
!  Negate if appropriate
!
      if (intg < 0) then
         beg = beg - 1
         inttostr(beg:beg) = '-'
      end if
!
!  Add commas if requested, appropriate, and possible
!
      if (comi == 1 .and. leni > 3) then
         ncom = (leni - 1)/3
         if (ncom < beg) then
            last = lens
            do n = 1, ncom
               curr = last - 2
               inttostr((beg - 1):lens) = inttostr(beg:(curr - 1))//',' &
     &                                    //inttostr(curr:lens)
               beg = beg - 1
               last = curr - 2
            end do
            como = 1
         end if
      end if
!
      end function inttostr