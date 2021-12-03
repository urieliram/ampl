!
      MODULE cplex_forman
!
!  Module contains Fortran 90 routines that call CPLEX 6.0 routines that
!   require special handling - eg, functions that receive and/or return
!   arrays of strings - see the information header section of module
!   'cplex_ifaces' for a summary of the special handling procedures - this
!   version is for incorporation in a DLL.
!
!  This version requires that all NULL arguments except string arguments
!   at the end the function argument list in the applicable CPLEX 6.0
!   functions (see below) be present in the calls to the corresponding
!   Fortran routines in this module.  See module version 'cplex_foropt'
!   for routines that allow these arguments to be optional but which do
!   not work for unknown reasons.
!
!  Currently 10 CPLEX 6.0 functions have counterpart routines in this module
!   as shown below - reasons for using the module routines are shown in
!   parentheses and 'F:' precedes the names of Fortran function procedures and
!   'S:' precedes the names of Fortran subroutine procedures
!
!   1. 'CPXaddcols'           -> Called with module routine:
!   F: 'addcolsCPX'              (receive character string array)
!
!   2. 'CPXaddrows'           -> Called with module routine:
!   F: 'addrowsCPX'              (receive character string arrays)
!
!   3. 'CPXcheckaddcols'      -> Called with module routine:
!   F: 'checkaddcolsCPX'         (receive character string array)
!
!   4. 'CPXcheckaddrows'      -> Called with module routine:
!   F: 'checkaddrowsCPX'         (receive character string arrays)
!
!   5. 'CPXcheckcopylpwnames' -> Called with module routine:
!   F: 'checkcopylpwnamesCPX'    (receives character string arrays)
!
!   6. 'CPXcopylpwnames'      -> Called with module routine:
!   F: 'copylpwnamesCPX'         (receive character string arrays)
!
!   7. 'CPXgetcolname'        -> Called with module routine:
!   S: 'getcolnameCPX'           (return character string array)
!
!   8. 'CPXgetrowname'        -> Called with module routine:
!   S: 'getrownameCPX'           (return character string array)
!
!   9. 'CPXnewcols'           -> Called with module routine:
!   F: 'newcolsCPX'              (receive character string array)
!
!  10. 'CPXnewrows'           -> Called with module routine:
!   F: 'newrowsCPX'              (receive character string array)
!
!  11. 'CPXaddqconstr'        -> Called with module routine:
!   F: 'addqconstrCPX'           (receive character string arrays)
!
!  References:
!            A.  'Using the CPLEX Callable Library', Version 5.0,
!                Copyright 1997 ILOG, Inc., CPLEX Division, Incline Village,
!                NV
!
!            B.  'CPLEX 6.0 Documentation Supplement', Supplement to the
!                CPLEX 5.0 Documentation (Reference A), Copyright 1998 ILOG,
!                Inc., CPLEX Division, Incline Village, NV
!
!  Written By:  Ike Patterson                       On:  26 May 98
!
!  Last Modified On:  30 May 98
!
!  Incorporate the symbolic types module
!
      use symtypes
!
!  Indicate private routines (found at the end of the module)
!
      PRIVATE :: eqllennames, eqlsublen, unqlennames
!
!  Special Fortran 90 routines
!
      CONTAINS
!
!  PUBLIC routines - the routines to call CPLEX 6.0 functions
!
!  Function 'addcolsCPX' to add columns to the current problem with
!   'CPXaddcols'
!
      function addcolsCPX(env,lpid,ccnt,nzcnt,obj,cmatbeg,cmatind,      &
     &                    cmatval,lobnds,upbnds,colname)
!
!DEC$ ATTRIBUTES DLLEXPORT :: addcolsCPX
!
!  NOTE:  All array indexing in this routine starts at one and refer to
!          columns and rows indexed in CPLEX starting at 0 as in standard C
!
!  Input arguments:
!               env : integer environment ID
!              lpid : integer ID for current problem
!              ccnt : integer number of new columns 
!             nzcnt : integer number of new nonzero coefficients to add
!                      to the technical coefficient matrix
!               obj : double precision vector which has length either
!                      1) 'ccnt' in which case 'obj' contains the objective
!                      function coefficients for the new columns or 2) 0
!                      which simulates a C NULL pointer so that the objective
!                      function coefficients for all new columns are set to
!                      zero
!           cmatbeg : integer vector of length 'ccnt' that contains the
!                      first entry in 'cmatval' that corresponds to the
!                      indexed column
!           cmatind : integer vector of length 'nzcnt' that contains row
!                      indicies for the corresponding column (all entries
!                      stored contiguously for each column)
!           cmatval : double precision vector of length 'nzcnt' that contains
!                      the technical matrix coefficients for the corresponding
!                      column (all entries stored contiguously for each
!                      column)
!            lobnds : double precision vector which has length either
!                      1) 'ccnt' in which case 'lobnds' contains the lower
!                      bounds for the new columns or 2) 0 which simulates a C
!                      NULL pointer so that the lower bounds for all new
!                      columns are set to zero
!            upbnds : double precision vector which has length either
!                      1) 'ccnt' in which case 'upbnds' contains the upper
!                      bounds for the new columns or 2) 0 which simulates a C
!                      NULL pointer so that the upper bounds for all new
!                      columns are set to positive infinity (CPX_INFBOUND)
!           colname : OPTIONAL character string vector which if present has
!                      length either 1) 'ccnt' in which case 'colname'
!                      contains the names of the new columns or 2) 0 in which
!                      case 'colname' is treated just as when absent - no
!                      names are assigned to the new columns
!
!  Output Result:
!        addcolsCPX : integer status flag returned by call to 'CPXaddcols'
!                      unless negative in which case the return value
!                      idicates:
!                        -1 -> 'ccnt' < 1
!                        -2 -> not enough memory for names' addresses arrays
!                        -3 -> 'colname' is present and nonempty (size > 0)
!                               but its size doesn't equal 'ccnt'
!
      use cplex_ifaces, only: CPXaddcols
!
      implicit none
!
!  Declare input/output arguments
!
      integer(IL) :: addcolsCPX
!
      integer(IL), intent(in) :: env, lpid, ccnt, nzcnt
      integer(IL), intent(in) :: cmatbeg(ccnt), cmatind(nzcnt)
!
      real(RD), intent(in) :: cmatval(nzcnt)
      real(RD), intent(in) :: obj(*), lobnds(*), upbnds(*)
!
      character(*), intent(in), optional :: colname(:)
!
!  Declare local arguments
!
      integer(IL) :: addstat, csize, n
!
      integer(IL), allocatable :: coladr(:)
!
!  Return if 'ccnt' is less than 1
!
      if (ccnt < 1) then
         addcolsCPX = -1
         return
      end if
!
!  Determine if column names are present and initialize 'csize' accordingly
!
      csize = 0
      if (present(colname)) csize = size(colname)
!
!  Allocate the names' addresses array and store the names' addresses if
!   appropriate - return if not enough memory or if the name array is present
!   but of length not equal to 'ccnt'
!
      allocate(coladr(csize),stat = addstat)
!
      if (addstat /= 0) then
         addcolsCPX = -2
         return
      end if
!
      if (csize > 0) then
         if (csize /= ccnt) then
            addcolsCPX = -3
            deallocate(coladr)
            return
         end if
         do n = 1, ccnt
            coladr(n) = loc(colname(n))
         end do
      end if
!
!  Call 'CPXaddcols' to add the columns
!
      addcolsCPX = CPXaddcols(env,lpid,ccnt,nzcnt,obj,cmatbeg,cmatind,  &
     &                        cmatval,lobnds,upbnds,coladr)
!
!  Deallocate the addresses array and return
!
      deallocate(coladr)
!
      end function addcolsCPX
!
!  Function 'addrowsCPX' to add rows to the current problem with
!   'CPXaddrows'
!
      function addrowsCPX(env,lpid,ccnt,rcnt,nzcnt,rhs,sense,rmatbeg,   &
     &                    rmatind,rmatval,colname,rowname)
!
!DEC$ ATTRIBUTES DLLEXPORT :: addrowsCPX
!
!  NOTE:  All array indexing in this routine starts at one and refer to
!          columns and rows indexed in CPLEX starting at 0 as in standard C
!
!  Input arguments:
!               env : integer environment ID
!              lpid : integer ID for current problem
!              ccnt : integer number of new columns 
!              rcnt : integer number of new rows
!             nzcnt : integer number of new nonzero coefficients to add
!                      to the technical coefficient matrix
!               rhs : double precision vector which has length either
!                      1) 'rcnt' in which case 'rhs' contains the RHS values
!                      for the new rows or 2) 0 in which case 'rhs' simulates
!                      a C NULL pointer and all new rows are assigned RHS
!                      values of zero
!             sense : single character vector which has length either
!                      1) 'rcnt' in which case 'sense' contains the senses
!                      for the new rows or 2) 0 in which case 'sense'
!                      simulates a C NULL pointer and all new rows are
!                      treated as equations
!           rmatbeg : integer vector of length 'rcnt' that contains the
!                      first entry in 'rmatval' that corresponds to the
!                      indexed row
!           rmatind : integer vector of length 'nzcnt' that contains column
!                      indicies for the corresponding row (all entries stored
!                      contiguously for each row)
!           rmatval : double precision vector of length 'nzcnt' that contains
!                      the technical matrix coefficients for the corresponding
!                      row (all entries stored contiguously for each row)
!           colname : OPTIONAL character string vector which if present has
!                      length either 1) 'ccnt' in which case 'colname'
!                      contains the names of the new columns or 2) 0 in which
!                      case 'colname' is treated just as when absent - no
!                      names are assigned to the new columns
!           rowname : OPTIONAL character string vector which if present has
!                      length either 1) 'rcnt' in which case 'rowname'
!                      contains the names of the new rows or 2) 0 in which
!                      case 'rowname' is treated just as when absent - no
!                      names are assigned to the new rows           
!
!  Output Result:
!        addrowsCPX : integer status flag returned by call to 'CPXaddrows'
!                      unless negative in which case the return value
!                      idicates:
!                        -1 -> 'rcnt' < 1
!                        -2 -> not enough memory for names' addresses arrays
!                        -3 -> 'colname' is present and nonempty (size > 0)
!                               but its size doesn't equal 'ccnt'
!                        -4 -> 'rowname' is present and nonempty (size > 0)
!                               but its size doesn't equal 'rcnt'
!
      use cplex_ifaces, only: CPXaddrows
!
      implicit none
!
!  Declare input/output arguments
!
      integer(IL) :: addrowsCPX
!
      integer(IL), intent(in) :: ccnt, rcnt, nzcnt
      integer(ILL), intent(in) :: env, lpid
      integer(IL), intent(in) :: rmatbeg(rcnt), rmatind(nzcnt)
!
      real(RD), intent(in) :: rmatval(nzcnt)
      real(RD), intent(in) :: rhs(*)
!
      character(1), intent(in):: sense(*)
      character(15), intent(in), optional :: colname(*), rowname(*)
!
!  Declare local arguments
!
      integer(IL) :: addstat, csize, n, rsize
!
      integer(IL), allocatable :: coladr(:), rowadr(:)
!
!  Return if 'rcnt' is less than 1
!
!  Call 'CPXaddrows' to add the rows
!
      addrowsCPX = CPXaddrows(env,lpid,ccnt,rcnt,nzcnt,rhs,sense,       &
     &                        rmatbeg,rmatind,rmatval,coladr,rowadr)
!
!  Deallocate the addresses arrays and return
!
!      deallocate(coladr,rowadr)
!
      end function addrowsCPX


      function addqconstrCPX(env,lpid,linzcnt,quadnzcnt,rhsval,sense,&
     &                       linind,linval,quadrow,quadcol,quadval,lname_str)
!
!DEC$ ATTRIBUTES DLLEXPORT :: addqconstrCPX
!
!  NOTE:  All array indexing in this routine starts at one and refer to
!          columns and rows indexed in CPLEX starting at 0 as in standard C
!
!  Input arguments:
!linnzcnt An integer that indicates the number of nonzero constraint coefficients in the linear part of the constraint. This specifies the length of the arrays linind and linval. 
 
!quadnzcnt An integer that indicates the number of nonzero constraint coefficients in the quadratic part of the constraint. This specifies the length of the arrays quadrow, quadcol and quadval.
 
!rhs The righthand side term for the constraint to be added.
 
!sense The sense of the constraint to be added. Note that quadratic constraints may only be less-than-or-equal-to or greater-than-or-equal-to constraints. See the discussion of QCP in the CPLEX User's Manual. 
 
!linind An array that with linval defines the linear part of the quadratic constraint to be added.
 
!linval An array that with linind defines the linear part of the constraint to be added. The nonzero coefficients of the linear terms must be stored in sequential locations in the arrays linind and linval from positions 0 to linnzcnt-1. Each entry, linind[i], indicates the variable index of the corresponding coefficient, linval[i]. May be NULL; then the constraint will have no linear terms. 
 
!quadrow An array that with quadcol and quadval defines the quadratic part of the quadratic constraint to be added. 
 
!quadcol An array that with quadrow and quadval defines the quadratic part of the quadratic constraint to be added.
 
!quadval An array that with quadrow and quadcol define the quadratic part of the constraint to be added. The nonzero coefficients of the quadratic terms must be stored in sequential locations in the arrays quadrow, quadcol and quadval from positions 0 to quadnzcnt-1. Each pair, quadrow[i], quadcol[i], indicates the variable indices of the quadratic term, and quadval[i] the corresponding coefficient. 
 
!lname_str The name of the constraint to be added. May be NULL, in which case the new constraint is assigned a default name if the quadratic constraints already resident in the CPLEX problem object have names; otherwise, no name is associated with the constraint. 
 
!
!  Output Result:
!        addqconstrCPX : integer status flag returned by call to 'CPXaddqconstr'
!                        unless negative in which case the return value
!                        idicates:
!                        -1 -> 'rcnt' < 1
!                        -2 -> not enough memory for names' addresses arrays
!                        -3 -> 'colname' is present and nonempty (size > 0)
!                               but its size doesn't equal 'ccnt'
!                        -4 -> 'rowname' is present and nonempty (size > 0)
!                               but its size doesn't equal 'rcnt'
!
      use cplex_ifaces, only: CPXaddqconstr
!
      implicit none
!
!  Declare input/output arguments
!
      integer(IL) :: addqconstrCPX
!
      !integer(IL), intent(in) :: env, lpid, linzcnt, quadnzcnt
      integer(IL), intent(in) :: linzcnt, quadnzcnt
      integer(8), intent(in) :: env, lpid
      integer(IL), intent(in) :: linind(linzcnt)
      integer(IL), intent(in) :: quadrow(quadnzcnt), quadcol(quadnzcnt)
      real(RD), intent(in) :: rhsval, linval(linzcnt), quadval(quadnzcnt)
      character(1), intent(in) :: sense
      character(15), intent(in), optional :: lname_str
!
!  Declare local arguments
!
      integer(IL) :: addstat, csize, n, rsize
!
      integer(IL) :: rowadr
!
!  Return if 'rcnt' is less than 1
!
!  Call 'CPXaddrows' to add the rows
!
      addqconstrCPX = CPXaddqconstr(env,lpid,linzcnt,quadnzcnt,rhsval,sense,&
     &                           linind,linval,quadrow,quadcol,quadval,rowadr )
!
!  Deallocate the addresses arrays and return
!
!      deallocate(rowadr)
!
      end function addqconstrCPX

!
!
!  Function 'copylpwnamesCPX' to copy an LP with 'CPXcopylpwnames' similar
!   to 'CPXcopylp' but with the addition of optional column and row names
!
      function copylpwnamesCPX(env,lpid,ccnt,rcnt,objsen,obj,rhs,sense, &
     &                         matbeg,matcnt,matind,matval,lobnds,      &
     &                         upbnds,rngval,colname,rowname)
!
!DEC$ ATTRIBUTES DLLEXPORT :: copylpwnamesCPX
!
!  NOTE:  All array indexing in this routine starts at one and refer to
!          columns and rows indexed in CPLEX starting at 0 as in standard C
!
!  Input arguments:
!               env : integer environment ID
!              lpid : integer ID for current problem
!              ccnt : integer number of columns in the problem 
!              rcnt : integer number of rows in the problem
!            objsen : integer is -1 for a maximization problem or 1 (default)
!                      or a minimization problem
!               obj : double precision vector of length 'ccnt' containing the
!                      objective function coefficients
!               rhs : double precision vector of length 'rcnt' containing the
!                      RHS values for each constraint in the problem
!             sense : single character vector of length 'rcnt' containing the
!                      sense for each constraint in the problem
!                      1) 'rcnt' in which case 'sense' contains the senses
!            matbeg : integer vector of length 'ccnt' that contains the first
!                      entry in 'matind' and 'matval' that corresponds to the
!                      indexed column (components must be in ascending order)
!            matcnt : integer vector of length 'ccnt' that constains the number
!                      of entries in 'matind' and 'matval' corresponding to the
!                      indexed column
!            matind : integer vector of at least length necessary to contain
!                      the row indicies corresponding to each of the nonzero
!                      coefficient in the technical coefficient matrix
!            matval : double precision vector of at least length necessary to
!                      contain every nonzero coefficient in the technical
!                      coefficient matrix
!            lobnds : double precision vector of length 'ccnt' containing the
!                      lower bounds on the columns
!            upbnds : double precision vector of length 'ccnt' containing the
!                      upper bounds on the columns
!            rngval : double precision vector of length either 1) 'rcnt' in
!                      which case 'rngval' contains the range values for each
!                      constraint in the problem or 2) 0 in which case each
!                      constraint is assigned a range value of zero
!           colname : OPTIONAL character string vector which if present has
!                      length either 1) 'ccnt' in which case 'colname'
!                      contains the names of the new columns or 2) 0 in which
!                      case 'colname' is treated just as when absent - no
!                      names are assigned to the new columns
!           rowname : OPTIONAL character string vector which if present has
!                      length either 1) 'rcnt' in which case 'rowname'
!                      contains the names of the new rows or 2) 0 in which
!                      case 'rowname' is treated just as when absent - no
!                      names are assigned to the new rows           
!
!  Output Result:
!   copylpwnamesCPX : integer status flag returned by call to 'CPXcopylpwnames'
!                      unless negative in which case the return value
!                      idicates:
!                        -1 -> 'rcnt' < 1 (fatal)
!                        -2 -> not enough memory for names' addresses arrays
!                               (fatal)
!                        -3 -> 'colname' is present and nonempty (size > 0)
!                               but its size doesn't equal 'ccnt' (fatal)
!                        -4 -> 'rowname' is present and nonempty (size > 0)
!                               but its size doesn't equal 'rcnt' (fatal)
!                        -5 -> 'objsen' neither -1 nor 1, set to default of 1
!                                for minimization problem (warning) - if
!                                'CPXcopylpwnames' subsequently returns an 
!                                error indication value then the negative of
!                                that value is returned to indicate both
!                                occurrences
!
      use cplex_ifaces, only: CPXcopylpwnames
!
      implicit none
!
!  Declare input/output arguments
!
      integer(IL) :: copylpwnamesCPX
!
      integer(IL), intent(in) :: env, lpid, ccnt, rcnt, objsen
      integer(IL), intent(in) :: matbeg(ccnt), matcnt(ccnt), matind(*)
!
      real(RD), intent(in) :: obj(ccnt), rhs(rcnt), matval(*),          &
     &                        lobnds(ccnt), upbnds(ccnt), rngval(*)
!
      character(1), intent(in):: sense(rcnt)
      character(*), intent(in), optional :: colname(:), rowname(:)
!
!  Declare local arguments
!
      integer(IL) :: addstat, csize, n, rsize, tmprtn, usesen
!
      integer(IL), allocatable :: coladr(:), rowadr(:)
!
      logical(LD) :: invalid
!
!  Return if 'rcnt' is less than 1
!
      if (rcnt < 1) then
         copylpwnamesCPX = -1
         return
      end if
!
!  Determine if column and/or row names are present and initialize 'csize'
!   and 'rsize' accordingly
!
      csize = 0
      if (ccnt > 0 .and. present(colname)) csize = size(colname)
!
      rsize = 0
      if (present(rowname)) rsize = size(rowname)
!
!  Allocate the names' addresses arrays and store the names' addresses if
!   appropriate - return if not enough memory or incorrectly sized 'name'
!   arrays are input
!
      allocate(coladr(csize),rowadr(rsize),stat = addstat)
!
      if (addstat /= 0) then
         copylpwnamesCPX = -2
         return
      end if
!
      if (csize > 0) then
         if (csize /= ccnt) then
            copylpwnamesCPX = -3
            deallocate(coladr)
            if (allocated(rowadr)) deallocate(rowadr)
            return
         end if
         do n = 1, ccnt
            coladr(n) = loc(colname(n))
         end do
      end if
!
      if (rsize > 0) then
         if (rsize /= rcnt) then
            copylpwnamesCPX = -4
            deallocate(rowadr)
            if (allocated(coladr)) deallocate(coladr)
            return
         end if
         do n = 1, rcnt
            rowadr(n) = loc(rowname(n))
         end do
      end if
!
!  Set 'usesen' to 'objsen' and change to 1 for minimization problem if
!   'objsen' is invalid and set warning flag - if 'CPXcopylpwnames' returns
!   an error status indication, then the negative of that value will be
!   returned to indicate both occurrences
!
      if (objsen == -1 .or. objsen == 1) then
         usesen = objsen
         invalid = .false.
      else
         usesen = 1
         invalid = .true.
      end if
!
!  Call 'CPXcopylpwnames' to create (copy) the LP
!
      tmprtn = CPXcopylpwnames(env,lpid,ccnt,rcnt,usesen,obj,rhs,sense, &
     &                         matbeg,matcnt,matind,matval,lobnds,      &
     &                         upbnds,rngval,coladr,rowadr)
!
!  Deallocate the addresses arrays
!
      deallocate(coladr,rowadr)
!
!  Return proper value
!
      if (invalid) then
         if (tmprtn .gt. 0) then
            copylpwnamesCPX = -tmprtn
         else
            copylpwnamesCPX = -5
         end if
      else
         copylpwnamesCPX = tmprtn
      end if
!
      end function copylpwnamesCPX
!
!  Subroutine 'getcolnameCPX' to get specified column names for current
!   problem with 'CPXgetcolname'
!
      subroutine getcolnameCPX(env,lpid,begin,finish,curnum,status,     &
     &                         names,maxlen)
!
!DEC$ ATTRIBUTES DLLEXPORT :: getcolnameCPX
!
!  Input arguments:
!               env : integer environment ID
!              lpid : integer ID for current problem
!             begin : integer index for first desired column (C indexing)
!            finish : integer index for last desired column (C indexing)
!            curnum : integer value of the current number of columns in the
!                      problem
!
!  Output Arguments:
!            status : integer status flag - value returned by 'CPXgetcolname'
!                      if 'status' >= 0, else a negative integer is returned
!                      indicating:
!                        -1 -> 'finish' < 'begin' ==> no names requested
!                                (fatal)
!                        -2 -> 'begin' < 0 and/or 'finish' > 'curnum' - 1
!                                (fatal)
!                        -3 -> length of a 'names' string is less than 2 ==>
!                               vary few names can be unique
!                                (fatal)
!                        -4 -> requested number of names found but more names
!                               were availble in the scalar name string
!                               returned in the second call to 'CPXgetcolname'
!                                (warning)
!                        -5 -> end of scalar name string returned in second
!                               call to 'CPXgetcolname' reached prior to
!                               finding the requested number of names -
!                               remaining names are filled with blanks
!                                (warning)
!             names : character string array dimensioned
!                     (1:('finish' - 'begin' + 1) containing the column names
!                     - all C NULL termination characters have been removed
!            maxlen : integer indicating the maximum nonblank length of a
!                      name if 'maxlen' > 0, else if 'maxlen' = 0 indicating
!                      that no names are assigned to columns in this problem
!                      and user may assign whatever names desired, else
!                      'maxlen' is returned as a negative integer indicating:
!                       < 0 -> string length of 'names' elements not long
!                               enough to hold the column names and names are
!                               are returned truncated with |'maxlen'| the
!                               length of the longest name encountered
!
      use cplex_ifaces, only: CPXgetcolname
!
      implicit none
!
!  Declare input/output arguments
!
      !integer(IL), intent(in) :: env, lpid, begin, finish, curnum
      integer(IL), intent(in) :: begin, finish, curnum
      integer(8), intent(in) :: env, lpid
      integer(IL), intent(out) :: status, maxlen
!
      character(*), intent(out) :: names(finish - begin + 1)
!
!  Declare local arguments
!
      integer(IL) :: haslen, reqcol, reqspace
!
!  Return if error conditions present and determine available length of a
!   'names' string and continue
!
      if (finish < begin) then
         status = -1
         return
      else if (begin < 0 .or. finish > (curnum - 1)) then
         status = -2
         return
      end if
!
      haslen = len(names(1))
      if (haslen < 2) then
         status = -3
         return
      end if
!
!  Initialize output arguments and number of requested column names
!
      status = 0
      maxlen = 0
      names = ' '
      reqcol = finish - begin + 1
!
!  Use locally contained subroutine 'getcolspace' to obtain the required
!   storage space and return if CPLEX returns error indication
!
      call getcolspace
      if (status /= 0) then
         if (reqspace == 0) status = 0
         return
      end if
!
!  Return if no names have been assigned to columns for this problem
!
      if (reqspace == 0) return
!
!  Use locally contained subroutine 'getcolnames' to obtain the names and
!   return
!
      call getcolnames
!
!  Locally contained routines
!
         Contains
!
!  Get column names
!
         subroutine getcolnames
!
         implicit none
!
         character(1), parameter :: nullc = char(0)
!
         character(reqspace) :: namestr
!
         integer(IL) :: surp
         integer(IL) :: eqlsub(4), pname(reqcol)
!
         status = CPXgetcolname(env,lpid,pname,namestr,reqspace,surp,   &
     &                          begin,finish)
!
!  See if all names are of equal length using private module routine
!   'eqlsublen'
!
         eqlsub = eqlsublen(namestr,nullc)
!
!  Use private module routine 'extrctnames' to extract the names from
!   'namestr' if names are not of equal length or if 'namestr' begins with
!   one or more NULL characters, otherwise use module routine 'eqllennames'
!   to obtain the names
!
         if (eqlsub(1) == 0 .or. eqlsub(3) .gt. 0) then
            call unqlennames(namestr,reqspace,reqcol,haslen,status,     &
     &                       maxlen,names)
         else
            call eqllennames(pname,reqcol,eqlsub(1),eqlsub(2),status,   &
     &                       maxlen,names)
         end if
!
         end subroutine getcolnames
!
!  Get amount of space (bytes = # characters) required to hold requested
!   column names in a string
!
         subroutine getcolspace
!
         use cplex_cons, only: CPXERR_NEGATIVE_SURPLUS
!
         implicit none
!
         character(2), pointer :: nostr
         integer(IL), allocatable :: noname(:)
!
         nullify(nostr)
         allocate(noname(0))
!
         status = CPXgetcolname(env,lpid,noname,nostr,0,reqspace,begin, &
     &                          finish)
!
         deallocate(noname)
!
         if (status == CPXERR_NEGATIVE_SURPLUS) then
            status = 0
            reqspace = -reqspace
         end if
!
         end subroutine getcolspace
!
      end subroutine getcolnameCPX
!
!  Subroutine 'getrownameCPX' to get specified row names for current problem
!   with 'CPXgetrowname'
!
      subroutine getrownameCPX(env,lpid,begin,finish,curnum,status,     &
     &                         names,maxlen)
!
!DEC$ ATTRIBUTES DLLEXPORT :: getrownameCPX
!
!  Input arguments:
!               env : integer environment ID
!              lpid : integer ID for current problem
!             begin : integer index for first desired row (C indexing)
!            finish : integer index for last desired row (C indexing)
!            curnum : integer value of the current number of rows in the
!                      problem
!
!  Output Arguments:
!            status : integer status flag - value returned by 'CPXgetrowname'
!                      if 'status' >= 0, else a negative integer is returned
!                      indicating:
!                        -1 -> 'finish' < 'begin' ==> no names requested
!                                (fatal)
!                        -2 -> 'begin' < 0 and/or 'finish' > 'curnum' - 1
!                                (fatal)
!                        -3 -> length of a 'names' string is less than 2 ==>
!                               very few names can be unique
!                                (fatal)
!                        -4 -> requested number of names found but more names
!                               were availble in the scalar name string
!                               returned in the second call to 'CPXgetrowname'
!                                (warning)
!                        -5 -> end of scalar name string returned in second
!                               call to 'CPXgetrowname' reached prior to
!                               finding the requested number of names -
!                               remaining names are filled with blanks
!                                (warning)
!             names : character string array dimensioned
!                     (1:('finish' - 'begin' + 1) containing the row names
!                     - all C NULL termination characters have been removed
!            maxlen : integer indicating the maximum nonblank length of a
!                      name if 'maxlen' > 0, else if 'maxlen' = 0 indicating
!                      that no names are assigned to rows in this problem and
!                      user may assign whatever names desired, else 'maxlen'
!                      is returned as a negative integer indicating:
!                       < 0 -> string length of 'names' elements not long
!                               enough to hold the row names and names are
!                               returned truncated with |'maxlen'| the
!                               length of the longest name encountered
!
      use cplex_ifaces, only: CPXgetrowname
!
      implicit none
!
!  Declare input/output arguments
!
      integer(IL), intent(in) :: env, lpid, begin, finish, curnum
      integer(IL), intent(out) :: status, maxlen
!
      character(*), intent(out) :: names(finish - begin + 1)
!
!  Declare local arguments
!
      integer(IL) :: haslen, reqrow, reqspace
!
!  Return if error conditions present and determine available length of a
!   'names' string and continue
!
      if (finish < begin) then
         status = -1
         return
      else if (begin < 0 .or. finish > (curnum - 1)) then
         status = -2
         return
      end if
!
      haslen = len(names(1))
      if (haslen < 2) then
         status = -3
         return
      end if
!
!  Initialize output arguments and number of requested row names
!
      status = 0
      maxlen = 0
      names = ' '
      reqrow = finish - begin + 1
!
!  Use locally contained subroutine 'getrowspace' to obtain the required
!   storage space and return if CPLEX returns error indication
!
      call getrowspace
      if (status /= 0) then
         if (reqspace == 0) status = 0
         return
      end if
!
!  Return if no names have been assigned to rows for this problem
!
      if (reqspace == 0) return
!
!  Use locally contained subroutine 'getrownames' to obtain the names and
!   return
!
      call getrownames
!
!  Locally contained routines
!
         Contains
!
!  Get row names
!
         subroutine getrownames
!
         implicit none
!
         character(1), parameter :: nullc = char(0)
!
         character(reqspace) :: namestr
!
         integer(IL) :: surp
         integer(IL) :: eqlsub(4), pname(reqrow)
!
         status = CPXgetrowname(env,lpid,pname,namestr,reqspace,surp,   &
     &                          begin,finish)
!
!  See if all names are of equal length using private module routine
!   'eqlsublen'
!
         eqlsub = eqlsublen(namestr,nullc)
!
!  Use private module routine 'extrctnames' to extract the names from
!   'namestr' if names are not of equal length or if 'namestr' begins with
!   one or more NULL characters, otherwise use module routine 'eqllennames'
!   to obtain the names
!
         if (eqlsub(1) == 0 .or. eqlsub(3) .gt. 0) then
            call unqlennames(namestr,reqspace,reqrow,haslen,status,     &
     &                       maxlen,names)
         else
            call eqllennames(pname,reqrow,eqlsub(1),eqlsub(2),status,   &
     &                       maxlen,names)
         end if
!
         end subroutine getrownames
!
!  Get amount of space (bytes = # characters) required to hold requested row
!   names in a string
!
         subroutine getrowspace
!
         use cplex_cons, only: CPXERR_NEGATIVE_SURPLUS
!
         implicit none
!
         character(2), pointer :: nostr
         integer(IL), allocatable :: noname(:)
!
         nullify(nostr)
         allocate(noname(0))
!
         status = CPXgetrowname(env,lpid,noname,nostr,0,reqspace,begin, &
     &                          finish)
!
         deallocate(noname)
!
         if (status == CPXERR_NEGATIVE_SURPLUS) then
            status = 0
            reqspace = -reqspace
         end if
!
         end subroutine getrowspace
!
      end subroutine getrownameCPX
!
!  Function 'newcolsCPX' to add columns to the current problem with
!   'CPXnewcols' without adding any technical matrix coefficients
!
      function newcolsCPX(env,lpid,ccnt,obj,lobnds,upbnds,ctype,colname)
!
!DEC$ ATTRIBUTES DLLEXPORT :: newcolsCPX
!
!  NOTE:  All array indexing in this routine starts at one and refer to
!          columns and rows indexed in CPLEX starting at 0 as in standard C
!
!  Input arguments:
!               env : integer environment ID
!              lpid : integer ID for current problem
!              ccnt : integer number of new columns 
!               obj : double precision vector which has length either
!                      1) 'ccnt' in which case 'obj' contains the objective
!                      function coefficients for the new columns or 2) 0
!                      which simulates a C NULL pointer so that the objective
!                      function coefficients for all new columns are set to
!                      zero
!            lobnds : double precision vector which has length either
!                      1) 'ccnt' in which case 'lobnds' contains the lower
!                      bounds for the new columns or 2) 0 which simulates a C
!                      NULL pointer so that the lower bounds for all new
!                      columns are set to zero
!            upbnds : double precision vector which has length either
!                      1) 'ccnt' in which case 'upbnds' contains the upper
!                      bounds for the new columns or 2) 0 which simulates a C
!                      NULL pointer so that the upper bounds for all new
!                      columns are set to positive infinity (CPX_INFBOUND)
!             ctype : OPTIONAL single character vector which if present has
!                      length either 1) 'ccnt' in which case 'ctype' contains
!                      the column variable types indicator:
!                          'B' = CPX_BINARY     -> binary (0/1) variable
!                          'C' = CPX_CONTINUOUS -> continuous variable
!                          'I' = CPX_INTEGER    -> general integer variable
!                      or 2) 0 in which case 'ctype' is treated the same as
!                      when 'ctype' is absent - all new variables are
!                      added as continous ('C') types
!           colname : OPTIONAL character string vector which if present has
!                      length either 1) 'ccnt' in which case 'colname'
!                      contains the names of the new columns or 2) 0 in which
!                      case 'colname' is treated just as when absent - no
!                      names are assigned to the new columns
!
!  Output Result:
!        newcolsCPX : integer status flag returned by call to 'CPXnewcols'
!                      unless negative in which case the return value
!                      idicates:
!                        -1 -> 'ccnt' < 1
!                        -2 -> not enough memory for names' addresses arrays
!                        -3 -> 'colname' is present and nonempty (size > 0)
!                               but its size doesn't equal 'ccnt'
!                        -4 -> 'ctype' is present and nonempty (size > 0) but
!                               its size doesn't equal 'ccnt'
!
      use cplex_ifaces, only: CPXnewcols
      !use parauseg, only: enb
!
      implicit none
!
!  Declare input/output arguments
!
      integer(IL) :: newcolsCPX
!
      integer(IL), intent(in) :: ccnt
      integer(ILL), intent(in) :: lpid
      integer(ILL), intent(in) :: env
!
      real(RD), intent(in) :: obj(*), lobnds(*), upbnds(*)
!
      character(1), intent(in), optional :: ctype(*)
      character(15), intent(in), optional :: colname(*)
!
!  Declare local arguments
!
      integer(IL) :: addstat, csize, n, tsize
!
      integer(IL), allocatable :: coladr(:)
!
      character(1), allocatable :: ctypevec(:)
!
!  Return if 'ccnt' is less than 1
!
!  Call 'CPXnewcols' to add the new columns
!
      newcolsCPX = CPXnewcols(env,lpid,ccnt,obj,lobnds,upbnds,ctype, coladr)
!
!  Deallocate the addresses arrays and return
!
!      deallocate(coladr,ctypevec)
!
      end function newcolsCPX
!
!  Function 'newrowsCPX' to add rows to the current problem with
!   'CPXnewrows' without adding technical matrix coefficients
!
      function newrowsCPX(env,lpid,rcnt,rhs,sense,rngval,rowname)
!
!DEC$ ATTRIBUTES DLLEXPORT :: newrowsCPX
!
!  NOTE:  All array indexing in this routine starts at one and refer to
!          columns and rows indexed in CPLEX starting at 0 as in standard C
!
!  Input arguments:
!               env : integer environment ID
!              lpid : integer ID for current problem
!              rcnt : integer number of new rows
!               rhs : double precision vector which has length either
!                      1) 'rcnt' in which case 'rhs' contains the RHS values
!                      for the new rows or 2) 0 in which case 'rhs' simulates
!                      a C NULL pointer and all new rows are assigned RHS
!                      values of zero
!             sense : single character vector which has length either
!                      1) 'rcnt' in which case 'sense' contains the senses
!                      for the new rows or 2) 0 in which case 'sense'
!                      simulates a C NULL pointer and all new rows are
!                      treated as equations
!            rngval : double precision vector which has length either
!                      1) 'rcnt' in which case 'rngval' contains the ranges
!                      for the new rows or 2) 0 in which case 'rngval'
!                      simulates a C NULL pointer and all new rows are
!                      assigned range values of zero
!           rowname : OPTIONAL character string vector which if present has
!                      length either 1) 'rcnt' in which case 'rowname'
!                      contains the names of the new rows or 2) 0 in which
!                      case 'rowname' is treated just as when absent - no
!                      names are assigned to the new rows           
!
!  Output Result:
!        newrowsCPX : integer status flag returned by call to 'CPXnewrows'
!                      unless negative in which case the return value
!                      idicates:
!                        -1 -> 'rcnt' < 1
!                        -2 -> not enough memory for names' addresses array
!                        -4 -> 'rowname' is present and nonempty (size > 0)
!                               but its size doesn't equal 'rcnt'
!
      use cplex_ifaces, only: CPXnewrows
!
      implicit none
!
!  Declare input/output arguments
!
      integer(IL) :: newrowsCPX
!
      integer(IL), intent(in) :: env, lpid, rcnt
!
      real(RD), intent(in) :: rhs(*), rngval(*)
!
      character(1), intent(in) :: sense(*)
      character(*), intent(in), optional :: rowname(:)
!
!  Declare local arguments
!
      integer(IL) :: addstat, n, rsize
!
      integer(IL), allocatable :: rowadr(:)
!
!  Return if 'rcnt' is less than 1
!
      if (rcnt < 1) then
         newrowsCPX = -1
         return
      end if
!
!  Determine if row names are present and initialize 'rsize' accordingly
!
      rsize = 0
      if (present(rowname)) rsize = size(rowname)
!
!  Allocate the names' addresses array and store the names' addresses if
!   appropriate - return if not enough memory or incorrectly sized 'name'
!   array is input
!
      allocate(rowadr(rsize),stat = addstat)
!
      if (addstat /= 0) then
         newrowsCPX = -2
         return
      end if
!
      if (rsize > 0) then
         if (rsize /= rcnt) then
            newrowsCPX = -4
            return
         end if
         do n = 1, rcnt
            rowadr(n) = loc(rowname(n))
         end do
      end if
!
!  Call 'CPXnewrows' to add the rows
!
      newrowsCPX = CPXnewrows(env,lpid,rcnt,rhs,sense,rngval,rowadr)
!
!  Deallocate the addresses array and return
!
      deallocate(rowadr)
!
      end function newrowsCPX
!
!  PRIVATE ROUTINES
!
!  Subroutine 'eqllennames' to extract the column or row names from the
!   character  pointer array returned by the second call to either
!   'CPXgetcolname' or 'CPXgetrowname' in the module routine 'getcolnameCPX'
!   or 'getrownameCPX' - used only when all names are of equal length and
!   first name doesn't begin with one or more NULL characters
!
      subroutine eqllennames(pname,reqnum,length,actnum,status,maxlen,  &
     &                       names)
!
!  Input Arguments:
!             pname : interger vector of length 'reqnum' returned by the
!                      applicable CPLEX function as an array of character
!                      string pointers
!            reqnum : integer number of requested column or row names
!            length : integer length of each equal-length name
!            actnum : integer number of names actually represented by'pname'
!
!  Input/Output Arguments:
!            status : integer status flag - returns input value if no problems
!                      encountered, else returns a negative integer
!                      indicating:
!                        -4 -> requested number of names found but more names
!                               were availble in the scalar name string
!                               returned in the second call to 'CPXgetcolname'
!                                (warning)
!                        -5 -> end of scalar name string returned in second
!                               call to 'CPXgetcolname' reached prior to
!                               finding the requested number of names -
!                               remaining names are filled with blanks
!                                (warning)
!            maxlen : integer indicating the maximum nonblank length of a
!                      name if 'maxlen' > 0, else if 'maxlen' = 0 (which
!                      should be the input value) indicating that no names
!                      assigned to columns or rows in this problem and the
!                      user may assign whatever names desired, else 'maxlen'
!                      is returned as a negative integer indicating:
!                       < 0 -> string length of 'names' elements not long
!                               enough to hold the names and names are
!                               returned truncated with |'maxlen'| the
!                               length of the longest name encountered
!
!  Output Argument:
!             names : character string array dimensioned (1:'reqnum')
!                     containing the column or row names - all C NULL
!                     termination characters have been removed
!
      implicit none
!
!  Declare input/output arguments
!
      integer(IL), intent(in) :: reqnum, length, actnum
      integer(IL), intent(in) :: pname(reqnum)
      integer(IL), intent(inout) :: status, maxlen
!
      character(*), intent(out) :: names(reqnum)
!
!  Declare local variables
!
      character(length + 1) :: pntobj
      pointer(pntrtn,pntobj)
!
      integer(IL) :: avail, i, iend, uselen
!
!  Initialize names to all blanks and set values of 'status' and 'maxlen'
!
      names = ' '
!
      if (reqnum < actnum) then
         status = -4
         iend = reqnum
      else if (reqnum > actnum) then
         status = -5
         iend = actnum
      else
         iend = reqnum
      end if
!
      avail = len(names(1))
      if (length > avail) then
         uselen = avail
         maxlen = -length
      else
         uselen = length
         maxlen = length
      end if
!
!  Use the addresses in 'pname' to store the names in 'names', stripping all
!   NULL termination characters using the DVF integer pointer 'pntrn' with
!   base variable 'pntobj'
!
      do i = 1, iend
         pntrtn = pname(i)
         names(i)(1:uselen) = pntobj(1:uselen)
      end do
!
      end subroutine eqllennames
!
!  Function 'eqlsublen' returns a size 4 rank 1 array detailing the
!   results of checking to see if an input string of substrings separated
!   by a 1 character delimiter has all substrings of equal length.
!
      function eqlsublen(string,delim)
!
!  Input Arguments:
!            string : character string to be checked
!             delim : character used as a delimiter to separate substrings
!                      within 'string' - must be different from any other
!                      character of 'string' or erroneous results will be
!                      returned
!
!  Output Result:
!         eqlsublen : interger size 4 rank 1 array whose elements indicate
!                      by index:
!                       1: 0 if not all substrings are of equal length
!                           or a positive integer indicating that all
!                           substrings have the same length equal to the
!                           returned value - if 0 is returned then elements
!                           2 - 4 meaningless
!                       2 : number of equal length substrings
!                       3 : number of successive occurrences of 'delim' at
!                            the beginning of 'string' - search for
!                            substrings starts at 'eqlsublen(3)' + 1
!                       4 : number of successive occurrences of 'delim' at
!                            the end of 'string' - if 'L' equals the length
!                            of string, then the search for substrings stops
!                            at 'L' - 'eqlsublen(4)'
!
!  Declare Input/Output Arguments
!
      integer(IL) :: eqlsublen(4)
!
      character(1), intent(in) :: delim
      character(*), intent(in) :: string
!
!  Declare local variables
!
      character(len(string) + 1) :: search
!
      integer(IL) :: iend, istr, jdel, strlen, sublen
!
!  Initialize result
!
      eqlsublen = 0
!
!  Determine length of input string and return elements 3 and 4
!
      strlen = len(string)
!
      eqlsublen(3) = verify(string,delim)
!
      if (eqlsublen(3) == 0) return
!
      eqlsublen(4) = verify(string,delim,.true.)
!
      if (eqlsublen(3) == eqlsublen(4)) then
         if (strlen == 1) return
         eqlsublen(1) = 1
         eqlsublen(2) = 1
         eqlsublen(3) = eqlsublen(3) - 1
         eqlsublen(4) = strlen - eqlsublen(4)
         return
      end if
!
!  Put the search portion of 'string' into 'search' terminating 'search'
!   with 'delim' and set 'strlen' to the length of 'search' and finalize
!   the values of the last two elements
!
      iend = strlen
      strlen = eqlsublen(4) - eqlsublen(3) + 2
      search(1:strlen) = string(eqlsublen(3):eqlsublen(4))//delim
      eqlsublen(3) = eqlsublen(3) - 1
      eqlsublen(4) = iend - eqlsublen(4)
!
!  Now search 'search' to see if all substrings are of equal length,
!   returning as soon as it is determined that not all substrings are of
!   equal length
!
      sublen = index(search,delim)
      if (sublen == strlen) then
         eqlsublen(1) = sublen - 1
         eqlsublen(2) = 1
         return
      end if
!
      iend = sublen
      sublen = sublen - 1
      eqlsublen(2) = 1
      do
         istr = iend + 1
         if (istr >= strlen) exit
         jdel = index(search(istr:strlen),delim) - 1
         if (jdel /= sublen) return
         eqlsublen(2) = eqlsublen(2) + 1
         iend = istr + jdel
      end do
!
!  If here, then all substrings have equal length, so set element 1 and
!   return
!
      eqlsublen(1) = sublen
!
      end function eqlsublen
!
!  Subroutine 'unqlennames' to extract the column or row names in the scalar
!   name string returned by the second call to either 'CPXgetcolname' or
!   'CPXgetrowname' in the module routine 'getcolnameCPX' or 'getrownameCPX'
!
      subroutine unqlennames(sentstr,hasspace,reqnum,haslen,status,     &
     &                       maxlen,names)
!
!  Input Arguments:
!           sentstr : scalar character string returned by the second call to
!                      'CPXgetcolname' or 'CPXgetrowname' - contains all
!                      requested column/row names separated by the NULL
!                      character
!          hasspace : integer length of the string 'sentstr' returned by the
!                      first call to 'CPXgetcolname' and 'CPXgetrowname'
!            reqnum : integer number of names requested
!            haslen : integer length of a string in the string vector 'names'
!                      (see below)
!
!  Input/Output Arguments:
!            status : integer status flag - returns input value if no problems
!                      encountered, else returns a negative integer
!                      indicating:
!                        -4 -> requested number of names found but more names
!                               were availble in the scalar name string
!                               returned in the second call to 'CPXgetcolname'
!                                (warning)
!                        -5 -> end of scalar name string returned in second
!                               call to 'CPXgetcolname' reached prior to
!                               finding the requested number of names -
!                               remaining names are filled with blanks
!                                (warning)
!            maxlen : integer indicating the maximum nonblank length of a
!                      name if 'maxlen' > 0, else if 'maxlen' = 0 (which
!                      should be the input value) indicating that no names
!                      assigned to columns or rows in this problem and the
!                      user may assign whatever names desired, else 'maxlen'
!                      is returned as a negative integer indicating:
!                       < 0 -> string length of 'names' elements not long
!                               enough to hold the names and names are
!                               returned truncated with |'maxlen'| the
!                               length of the longest name encountered
!
!  Output Argument:
!             names : character string array dimensioned (1:'reqnum')
!                     containing the column or row names - all C NULL
!                     termination characters have been removed
!
      implicit none
!
!  Declare input/output arguments
!
      integer(IL), intent(in) :: hasspace, reqnum, haslen
      integer(IL), intent(inout) :: status, maxlen
!
      character(hasspace), intent(in) :: sentstr
      character(*), intent(out) :: names(reqnum)
!
!  Declare local variables
!
      character(1) :: nulchr = char(0)
      character(hasspace + 1) :: namestr            
!
      integer(IL) :: iend, istr, jnul, nfnd, nlen, reqspace, shrt
!
      logical(LD) :: trun
!
      reqspace = hasspace
      namestr(1:reqspace) = sentstr
!
      if (namestr(reqspace:reqspace) /= nulchr) then
         reqspace = reqspace + 1
         namestr(reqspace:reqspace) = nulchr
      end if
!
      if (namestr(1:1) == nulchr) then
         namestr(1:(reqspace - 1)) = namestr(2:reqspace)
         reqspace = reqspace - 1
      end if
!
      iend = 0
      nfnd = 0
      trun = .false.
      shrt = haslen - 1
      do
         istr = iend + 1
         if (istr >= reqspace) exit
         jnul = index(namestr(istr:reqspace),nulchr)
         iend = istr + jnul - 1
         if (jnul == 1) cycle
         nfnd = nfnd + 1
         nlen = iend - istr
         if (nlen <= haslen) then
            names(nfnd)(1:nlen) = namestr(istr:(iend - 1))
         else
            trun = .true.
            names(nfnd) = namestr(istr:(istr + shrt))
         end if
         maxlen = max0(maxlen,nlen)
         if (nfnd == reqnum) then
            if (iend < reqspace) then
               do istr = (iend + 1), (reqspace - 1)
                  if (namestr(istr:istr) /= nulchr) then
                     status = -4
                     exit
                  end if
               end do
            end if
            exit
         end if
      end do
!
      if (trun) maxlen = -maxlen
      if (nfnd < reqnum) status = -5
!
      end subroutine unqlennames
!
      END MODULE cplex_forman