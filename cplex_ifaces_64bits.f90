      MODULE cplex_ifaces
!
!  Interfaces to the CPLEX Version 6.0 functions in alphabetic order for use
!   with Digital Visual Fortran 5.0 (DVF) - note that these interfaces will
!   also work with Microsoft Fortran Powerstation 4.0 if all metacommand
!   prefixes are changed from !DEC$ to !MS$
!
!  NOTE:  CPLEX functions which have character string arrays (char **pntname)
!          have interfaces showing these arguments as 'integer(IL)' since
!          Fortran 90 and C will not directly transfer character string arrays
!          (they will transfer scalar character strings) - in order to access
!          such CPLEX functions from DVF (note that the procedures summarized
!          below are encapsulated in Fortran 90 routines contained in module
!          'cplex_forrou' so that the applicable procedures do not need to be
!          rewritten each time an applicable CPLEX function is needed):
!
!         1.  CPLEX function receives character string array(s) -
!
!         In the Fortran 90 code, define applicable character arrays
!          [character(length), allocatable :: name_array(:)] and corresponding
!          integer arrays [integer(IL), allocatable :: name_addr(:)], allocate
!          the necessary memory for each array (say N names), use the DVF
!          'loc' function to define the integer arrays with the addresses of
!          each name string (remember to finish each string with the NULL
!          character):
!
!                        do i = 1, N
!                           name_addr(i) = loc(name_array(i))
!                        end do
!
!          and send the name_addr array to the CPLEX function.  See code
!          'lpex1f.f90' for an example using 'CPXaddrows'.  (Also note that
!          the maximum allowed length of a CPLEX name is 16 characters
!          according to paragraph 'Character Strings' on page 161 of Reference
!          A below although 'CPXaddrows' will accept and use much longer names
!          - it is probably not a good idea to rely on this and one should
!          instead plan on maximum length of 16 characters) 
!
!         2.  CPLEX function returns character string array(s) and each string
!              in the array has the same length - 
!
!         In the Fortran 90 code, define applicable character arrays
!          [character(length), allocatable :: name_array(:)] and corresponding
!          integer arrays [integer(IL), allocatable :: name_addr(:)], allocate
!          the necessary memory for each array (say N names), also declare
!          a scalar character variable of sufficient length to hold the 
!          expected largest name ( see last sentence in 1. above) (say objchr)
!          and make this the pointed-to object for a DVF integer pointer
!          [pntobj - note that the DVF integer pointer is not the same as a
!          standard Fortran 90 pointer but is a Digital (& Microsoft)
!          extension].
!
!                        pointer (pntobj, objchr)
!
!          Use the integer array, name_addr, as the character string array
!          argument in the CPLEX function call.  Upon return, name_array
!          will contain the addresses to the name strings which can then
!          be deferenced with the integer pointer to define the names array
!          name_array:
! 
!                        do i = 1, N
!                           pntobj = name_addr(i)
!                           name_addr(i) = objchr
!                        end do
!
!          and then the name strings may be modified as desired (such as
!          removing the terminating NULL characters attached to all strings
!          in C).  See code 'lpex1f.f90' for an example using 'CPXgetcolname'
!          and 'CPXgetrowname'.
!
!         3.  CPLEX function returns character string array(s) and strings in
!              the array are of different (or unknown) lengths -
!
!         Proceed as in 2 above, but don't attempt to declare/use the DVF
!          pointer arguments (pntobj and objchr).  Instead, CPLEX will also
!          return all strings in a scalar string argument (in addition to
!          the string array) separated by the NULL character.  The desired
!          strings may be extracted from this string (see routines
!          'getcolnameCPX' and 'getrownameCPX' in module 'cplex-forrou') 
!
!         Currently, 10 of the functions interfaced in this module fall in this
!          category and each has a Fortran 90 counter part in module
!          'cplex_forman' - 'F:' precedes the names of Fortran 90 function
!          procedures and 'S:' precedes the names of Fortran 90 subroutines,
!          and the reason for using the Fortran 90 routine to reference the
!          indicated CPLEX function is shown in parentheses:
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
!  Written By:  Ike Patterson                    On:  19 May 98
!
!  Last Modified On:   7 Jun 98
!
!  Allow no implicit declarations
!
      implicit none
!
!  CPLEX functions' interfaces
!
      INTERFACE
      function CPXaddchannel(env)
!DEC$ ATTRIBUTES stdcall, alias:'CPXaddchannel' :: CPXaddchannel
!DEC$ ATTRIBUTES value :: env
      use symtypes
      integer(IL) :: CPXaddchannel
      integer(IL), intent(in) :: env
      end function CPXaddchannel
      END INTERFACE
!
      INTERFACE
      function CPXaddcols(env,lpid,ccnt,nzcnt,objcoe,cmatbeg,cmatind,   &
     &                    cmatval,lobnds,upbnds,colnameadr)
!DEC$ ATTRIBUTES stdcall, alias:'CPXaddcols' :: CPXaddcols
!DEC$ ATTRIBUTES value :: env, lpid, ccnt, nzcnt
!DEC$ ATTRIBUTES reference :: objcoe, cmatbeg, cmatind, cmatval
!DEC$ ATTRIBUTES reference :: lobnds, upbnds, colnameadr
      use symtypes
      integer(IL) :: CPXaddcols
      integer(IL), intent(in) :: env, lpid, ccnt, nzcnt
      integer(IL), intent(in) :: cmatbeg(ccnt), cmatind(nzcnt)
      integer(IL), intent(in) :: colnameadr(*)
      real(RD), intent(in) :: objcoe(*), cmatval(nzcnt),lobnds(*),      &
     &                        upbnds(*)
      end function CPXaddcols
      END INTERFACE
!
      INTERFACE
      function CPXaddfpdest(env,channel,filepnt)
!DEC$ ATTRIBUTES stdcall, alias:'CPXaddfpdest' :: CPXaddfpdest
!DEC$ ATTRIBUTES value :: env, channel
!DEC$ ATTRIBUTES reference :: filepnt
      use symtypes
      integer(IL) :: CPXaddfpdest
      integer(IL), intent(in) :: env, channel
      integer(IL), intent(inout) :: filepnt
      end function CPXaddfpdest
      END INTERFACE
!
      INTERFACE
      function CPXaddfuncdest(env,channel,handlepnt,funcpnt)
!DEC$ ATTRIBUTES stdcall, alias:'CPXaddfuncdest' :: CPXaddfuncdest
!DEC$ ATTRIBUTES value :: env, channel
!DEC$ ATTRIBUTES reference :: handlepnt, funcpnt
      use symtypes
      integer(IL) :: CPXaddfuncdest
      integer(IL), intent(in) :: env, channel, handlepnt, funcpnt
      end function CPXaddfuncdest
      END INTERFACE
!
      INTERFACE
      function CPXaddrows(env,lpid,ccnt,rcnt,nzcnt,rhsval,sense,rmatbeg,&
     &                    rmatind,rmatval,colnameadr,rownameadr)
!DEC$ ATTRIBUTES stdcall, alias:'CPXaddrows' :: CPXaddrows
!DEC$ ATTRIBUTES value :: env, lpid, ccnt, rcnt, nzcnt
!DEC$ ATTRIBUTES reference :: rhsval, sense, rmatbeg, rmatind, rmatval
!DEC$ ATTRIBUTES reference :: colnameadr, rownameadr
      use symtypes
      integer(IL) :: CPXaddrows
      integer(IL), intent(in) :: ccnt, rcnt, nzcnt
      integer(ILL), intent(in) :: env, lpid
      integer(IL), intent(in) :: rmatbeg(rcnt), rmatind(nzcnt)
      integer(IL), intent(in) :: colnameadr(*), rownameadr(*)
      real(RD), intent(in) :: rhsval(*), rmatval(nzcnt)
      character(1), intent(in) :: sense(*)
      end function CPXaddrows
      END INTERFACE
!
      INTERFACE
      function CPXaddqconstr(env,lpid,linzcnt,quadnzcnt,rhsval,sense,&
     &                       linind,linval,quadrow,quadcol,quadval,lname_str)
!DEC$ ATTRIBUTES stdcall, alias:'CPXaddqconstr' :: CPXaddqconstr
!DEC$ ATTRIBUTES value :: env, lpid, linzcnt, quadnzcnt, rhsval, sense
!DEC$ ATTRIBUTES reference :: linind, linval
!DEC$ ATTRIBUTES reference :: quadrow, quadcol, quadval
!DEC$ ATTRIBUTES value :: lname_str
      use symtypes
      integer(IL) :: CPXaddqconstr
      integer(IL), intent(in) :: linzcnt, quadnzcnt
      integer(ILL), intent(in) :: env, lpid
      integer(IL), intent(in) :: linind(linzcnt)
      integer(IL), intent(in) :: quadrow(quadnzcnt), quadcol(quadnzcnt)
      integer(IL), intent(in) :: lname_str
      real(RD), intent(in) :: rhsval, linval(linzcnt), quadval(quadnzcnt)
      character(1), intent(in) :: sense
      end function CPXaddqconstr
      END INTERFACE
!
      INTERFACE
      function CPXbaropt(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXbaropt' :: CPXbaropt
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXbaropt
      integer(ILL), intent(in) :: env, lpid
      end function CPXbaropt
      END INTERFACE
!
      INTERFACE
      function CPXboundsa(env,lpid,begin,finish,lblower,lbupper,        &
     &                    ublower,ubupper)
!DEC$ ATTRIBUTES stdcall, alias:'CPXboundsa' :: CPXboundsa
!DEC$ ATTRIBUTES value :: env, lpid, begin, finish
!DEC$ ATTRIBUTES reference :: lblower, lbupper, ublower, ubupper
      use symtypes
      integer(IL) :: CPXboundsa
      integer(IL), intent(in) :: env, lpid, begin, finish
      real(RD), intent(out) :: lblower(*), lbupper(*), ublower(*),      &
     &                         ubupper(*)
      end function CPXboundsa
      END INTERFACE
!
!
      INTERFACE
      function CPXcheckchgcoeflist(env,lpid,numcoefs,rowlist,collist,   &
     &                             vallist)
!DEC$ ATTRIBUTES stdcall, alias:'CPXcheckchgcoeflist' :: CPXcheckchgcoeflist
!DEC$ ATTRIBUTES value :: env, lpid, numcoefs
!DEC$ ATTRIBUTES reference :: rowlist, collist, vallist
      use symtypes
      integer(IL) :: CPXcheckchgcoeflist
      integer(IL), intent(in) :: env, lpid, numcoefs
      integer(IL), intent(in) :: rowlist(*), collist(*)
      real(RD), intent(in) :: vallist(*)
      end function CPXcheckchgcoeflist
      END INTERFACE
!
      INTERFACE
      function CPXcheckcopylp(env,lpid,numcols,numrows,objsen,objcoe,   &
     &                        rhsval,sense,matbeg,matcnt,matind,matval, &
     &                        lobnds,upbnds,rngval)
!DEC$ ATTRIBUTES stdcall, alias:'CPXcheckcopylp' :: CPXcheckcopylp
!DEC$ ATTRIBUTES value :: env, lpid, numcols, numrows, objsen
!DEC$ ATTRIBUTES reference :: objcoe, rhsval, sense, matbeg, matcnt
!DEC$ ATTRIBUTES reference :: matind, matval, lobnds, upbnds, rngval
      use symtypes
      integer(IL) :: CPXcheckcopylp
      character(1), intent(in) :: sense(*)
      integer(IL), intent(in) :: env, lpid, numcols, numrows, objsen
      integer(IL), intent(in) :: matbeg(*), matcnt(*), matind(*)
      real(RD), intent(in) :: objcoe(*), rhsval(*), matval(*),lobnds(*),&
     &                        upbnds(*), rngval(*)
      end function CPXcheckcopylp
      END INTERFACE
!
!
      INTERFACE
      function CPXcheckcopyqpsep(env,lpid,qsepvec)
!DEC$ ATTRIBUTES stdcall, alias:'CPXcheckcopyqpsep' :: CPXcheckcopyqpsep
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: qsepvec
      use symtypes
      integer(IL) :: CPXcheckcopyqpsep
      integer(IL), intent(in) :: env, lpid
      real(RD), intent(in) :: qsepvec(*)
      end function CPXcheckcopyqpsep
      END INTERFACE
!
      INTERFACE
      function CPXcheckcopyquad(env,lpid,qmatbeg,qmatcnt,qmatind,       &
     &                          qmatval)
!DEC$ ATTRIBUTES stdcall, alias:'CPXcheckcopyquad' :: CPXcheckcopyquad
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: qmatbeg, qmatcnt, qmatind, qmatval
      use symtypes
      integer(IL) :: CPXcheckcopyquad
      integer(IL), intent(in) :: env, lpid
      integer(IL), intent(in) :: qmatbeg(*), qmatcnt(*), qmatind(*)
      real(RD), intent(in) :: qmatval(*)
      end function CPXcheckcopyquad
      END INTERFACE
!
      INTERFACE
      function CPXcheckvals(env,lpid,cnt,rowind,colind,values)
!DEC$ ATTRIBUTES stdcall, alias:'CPXcheckvals' :: CPXcheckvals
!DEC$ ATTRIBUTES value :: env, lpid, cnt
!DEC$ ATTRIBUTES reference :: rowind, colind, values
      use symtypes
      integer(IL) :: CPXcheckvals
      integer(IL), intent(in) :: env, lpid, cnt
      integer(IL), intent(in) :: rowind(*), colind(*)
      real(RD), intent(in) :: values(*)
      end function CPXcheckvals
      END INTERFACE
!
      INTERFACE
      function CPXchgbds(env,lpid,cnt,indices,bndtypes,bnds)
!DEC$ ATTRIBUTES stdcall, alias:'CPXchgbds' :: CPXchgbds
!DEC$ ATTRIBUTES value :: env, lpid, cnt
!DEC$ ATTRIBUTES reference :: indices, bndtypes, bnds
      use symtypes
      integer(IL) :: CPXchgbds
      integer(ILL), intent(in) :: env, lpid
      integer(IL), intent(in) :: cnt, indices(cnt)
      real(RD), intent(in) :: bnds(cnt)
      character(1), intent(in) :: bndtypes(cnt)
      end function CPXchgbds
      END INTERFACE
!
      INTERFACE
      function CPXchgcoef(env,lpid,rowind,colind,newvalue)
!DEC$ ATTRIBUTES stdcall, alias:'CPXchgcoef' :: CPXchgcoef
!DEC$ ATTRIBUTES value :: env, lpid, rowind, colind, newvalue
      use symtypes
      integer(IL) :: CPXchgcoef
      integer(IL), intent(in) :: env, lpid, rowind, colind
      real(RD), intent(in) :: newvalue
      end function CPXchgcoef
      END INTERFACE
!
      INTERFACE
      function CPXchgcoeflist(env,lpid,numcoefs,rowlist,collist,vallist)
!DEC$ ATTRIBUTES stdcall, alias:'CPXchgcoeflist' :: CPXchgcoeflist
!DEC$ ATTRIBUTES value :: env, lpid, numcoefs
!DEC$ ATTRIBUTES reference :: rowlist, collist, vallist
      use symtypes
      integer(IL) :: CPXchgcoeflist
      integer(IL), intent(in) :: numcoefs
      integer(ILL), intent(in) :: env, lpid
      integer(IL), intent(in) :: rowlist(numcoefs), collist(numcoefs)
      real(RD), intent(in) :: vallist(numcoefs)
      end function CPXchgcoeflist
      END INTERFACE
!
      INTERFACE
      function CPXchgctype(env,lpid,cnt,indices,ctype)
!DEC$ ATTRIBUTES stdcall, alias:'CPXchgctype' :: CPXchgctype
!DEC$ ATTRIBUTES value :: env, lpid, cnt
!DEC$ ATTRIBUTES reference :: indices, ctype
      use symtypes
      integer(IL) :: CPXchgctype
      integer(IL), intent(in) :: env, lpid, cnt
      integer(IL), intent(in) :: indices(cnt)
      character(1), intent(in) :: ctype(cnt)
      end function CPXchgctype
      END INTERFACE
!
      INTERFACE
      function CPXchgname(env,lpid,key,idx,newname)
!DEC$ ATTRIBUTES stdcall, alias:'CPXchgname' :: CPXchgname
!DEC$ ATTRIBUTES value :: env, lpid, key, idx
!DEC$ ATTRIBUTES reference :: newname
      use symtypes
      integer(IL) :: CPXchgname
      character(1), intent(in) :: key
      character(*), intent(in) :: newname
      integer(IL), intent(in) :: env, lpid, idx
      end function CPXchgname
      END INTERFACE
!
      INTERFACE
      function CPXchgobj(env,lpid,cnt,indices,values)
!DEC$ ATTRIBUTES stdcall, alias:'CPXchgobj' :: CPXchgobj
!DEC$ ATTRIBUTES value :: env, lpid, cnt
!DEC$ ATTRIBUTES reference :: indices, values
      use symtypes
      integer(IL) :: CPXchgobj
      integer(IL), intent(in) :: cnt
      integer(ILL), intent(in) :: env, lpid
      integer(IL), intent(in) :: indices(cnt)
      real(RD), intent(in) :: values(cnt)
      end function CPXchgobj
      END INTERFACE
!
      INTERFACE
      function CPXchgobjsen(env,lpid,maxormin)
!DEC$ ATTRIBUTES stdcall, alias:'CPXchgobjsen' :: CPXchgobjsen
!DEC$ ATTRIBUTES value :: env, lpid, maxormin
      use symtypes
      integer(IL) :: CPXchgobjsen
      integer(IL), intent(in) :: maxormin
      integer(ILL), intent(in) :: env, lpid
      end function CPXchgobjsen
      END INTERFACE
!
      INTERFACE
      function CPXchgprobtype(env,lpid,newtype)
!DEC$ ATTRIBUTES stdcall, alias:'CPXchgprobtype' :: CPXchgprobtype
!DEC$ ATTRIBUTES value :: env, lpid, newtype
      use symtypes
      integer(ILL) :: CPXchgprobtype
      integer(ILL), intent(in) :: newtype
      integer(ILL), intent(in) :: env, lpid
      end function CPXchgprobtype
      END INTERFACE
!
      INTERFACE
      function CPXchgqpcoef(env,lpid,rowind,colind,newvalue)
!DEC$ ATTRIBUTES stdcall, alias:'CPXchgqpcoef' :: CPXchgqpcoef
!DEC$ ATTRIBUTES value :: env, lpid, rowind, colind, newvalue
      use symtypes
      integer(IL) :: CPXchgqpcoef
      integer(IL), intent(in) :: env, lpid, rowind, colind
      real(RD), intent(in) :: newvalue
      end function CPXchgqpcoef
      END INTERFACE
!
      INTERFACE
      function CPXchgrhs(env,lpid,cnt,indices,values)
!DEC$ ATTRIBUTES stdcall, alias:'CPXchgrhs' :: CPXchgrhs
!DEC$ ATTRIBUTES value :: env, lpid, cnt
!DEC$ ATTRIBUTES reference :: indices, values
      use symtypes
      integer(IL) :: CPXchgrhs
      integer(IL), intent(in) :: cnt
      integer(ILL), intent(in) :: env, lpid
      integer(IL), intent(in) :: indices(cnt)
      real(RD), intent(in) :: values(cnt)
      end function CPXchgrhs
      END INTERFACE
!
      INTERFACE
      function CPXchgsense(env,lpid,cnt,indices,sense)
!DEC$ ATTRIBUTES stdcall, alias:'CPXchgsense' :: CPXchgsense
!DEC$ ATTRIBUTES value :: env, lpid, cnt
!DEC$ ATTRIBUTES reference :: indices, sense
      use symtypes
      integer(IL) :: CPXchgsense
      integer(IL), intent(in) :: env, lpid, cnt
      integer(IL), intent(in) :: indices(cnt)
      character(1), intent(in) :: sense(cnt)
      end function CPXchgsense
      END INTERFACE
!
      INTERFACE
      function CPXcloseCPLEX(env)
!DEC$ ATTRIBUTES stdcall, alias:'CPXcloseCPLEX' :: CPXcloseCPLEX
!DEC$ ATTRIBUTES reference :: env
      use symtypes
      integer(IL) :: CPXcloseCPLEX
      integer(ILL), intent(inout) :: env
      end function CPXcloseCPLEX
      END INTERFACE
!
      INTERFACE
      function CPXclpwrite (env,lpid,filename)
!DEC$ ATTRIBUTES stdcall, alias:'CPXclpwrite' :: CPXclpwrite
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXclpwrite
      character(*), intent(in) :: filename
      integer(ILL), intent(in) :: env, lpid
      end function CPXclpwrite
      END INTERFACE
!
      INTERFACE
      function CPXcopybase(env,lpid,cstat,rstat)
!DEC$ ATTRIBUTES stdcall, alias:'CPXcopybase' :: CPXcopybase
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: cstat, rstat
      use symtypes
      integer(IL) :: CPXcopybase
      integer(IL), intent(in) :: env, lpid
      integer(IL), intent(in) :: cstat(*), rstat(*)
      end function CPXcopybase
      END INTERFACE
!
      INTERFACE
      function CPXcopyctype(env,lpid,ctype)
!DEC$ ATTRIBUTES stdcall, alias:'CPXcopyctype' :: CPXcopyctype
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: ctype
      use symtypes
      integer(IL) :: CPXcopyctype
      character(1), intent(in) :: ctype
      integer(IL), intent(in) :: env, lpid
      end function CPXcopyctype
      END INTERFACE
!
      INTERFACE
      function CPXcopylp(env,lpid,numcols,numrows,objsen,objcoe,rhsval, &
     &                   sense,matbeg,matcnt,matind,matval,lobnds,      &
     &                   upbnds,rngval)
!DEC$ ATTRIBUTES stdcall, alias:'CPXcopylp' :: CPXcopylp
!DEC$ ATTRIBUTES value :: env, lpid, numcols, numrows, objsen
!DEC$ ATTRIBUTES reference :: objcoe, rhsval, sense, matbeg, matcnt
!DEC$ ATTRIBUTES reference :: matind, matval, lobnds, upbnds, rngval
      use symtypes
      integer(IL) :: CPXcopylp
      character(1), intent(in) :: sense(*)
      integer(IL), intent(in) :: env, lpid, numcols, numrows, objsen
      integer(IL), intent(in) :: matbeg(*), matcnt(*), matind(*)
      real(RD), intent(in) :: objcoe(*), rhsval(*), matval(*),lobnds(*),&
     &                        upbnds(*), rngval(*)
      end function CPXcopylp
      END INTERFACE
!
      INTERFACE
      function CPXcopylpwnames(env,lpid,numcols,numrows,objsen,objcoe,  &
     &                         rhsval,sense,matbeg,matcnt,matind,matval,&
     &                         lobnds,upbnds,rngval,colnameadr,         &
     &                         rownameadr)
!DEC$ ATTRIBUTES stdcall, alias:'CPXcopylpwnames' :: CPXcopylpwnames
!DEC$ ATTRIBUTES value :: env, lpid, numcols, numrows, objsen
!DEC$ ATTRIBUTES reference :: objcoe, rhsval, sense, matbeg, matcnt
!DEC$ ATTRIBUTES reference :: matind, matval, lobnds, upbnds, rngval
!DEC$ ATTRIBUTES reference :: colnameadr, rownameadr
      use symtypes
      integer(IL) :: CPXcopylpwnames
      integer(IL), intent(in) :: env, lpid, numcols, numrows, objsen
      integer(IL), intent(in) :: matbeg(numcols), matcnt(numcols),      &
     &                           matind(*), colnameadr(*), rownameadr(*)
      real(RD), intent(in) :: objcoe(numcols), rhsval(numrows),         &
     &                        matval(*), lobnds(numcols),               &
     &                        upbnds(numcols), rngval(*)
      character(1), intent(in) :: sense(numrows)
      end function CPXcopylpwnames
      END INTERFACE
!
      INTERFACE
      function CPXcopymipstart(env,lpid,cnt,indices,value)
!DEC$ ATTRIBUTES stdcall, alias:'CPXcopymipstart' :: CPXcopymipstart
!DEC$ ATTRIBUTES value :: env, lpid, cnt
!DEC$ ATTRIBUTES reference :: indices, value
      use symtypes
      integer(IL) :: CPXcopymipstart
      integer(IL), intent(in) :: env, lpid, cnt
      integer(IL), intent(in) :: indices(cnt)
      real(RD), intent(in) :: value(cnt)
      end function CPXcopymipstart
      END INTERFACE
!
      INTERFACE
      function CPXcopyobjname(env,lpid,objname)
!DEC$ ATTRIBUTES stdcall, alias:'CPXcopyobjname' :: CPXcopyobjname
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: objname
      use symtypes
      integer(IL) :: CPXcopyobjname
      character(*), intent(in) :: objname
      integer(IL), intent(in) :: env, lpid
      end function CPXcopyobjname
      END INTERFACE
!
      INTERFACE
      function CPXcopyorder(env,lpid,cnt,indices,priority,direction)
!DEC$ ATTRIBUTES stdcall, alias:'CPXcopyorder' :: CPXcopyorder
!DEC$ ATTRIBUTES value :: env, lpid, cnt
!DEC$ ATTRIBUTES reference :: indices, priority, direction
      use symtypes
      integer(IL) :: CPXcopyorder
      integer(IL), intent(in) :: env, lpid, cnt
      integer(IL), intent(in) :: indices(cnt), priority(cnt),           &
     &                           direction(cnt)
      end function CPXcopyorder
      END INTERFACE
!
      INTERFACE
      function CPXcopyqpsep(env,lpid,qsepvec)
!DEC$ ATTRIBUTES stdcall, alias:'CPXcopyqpsep' :: CPXcopyqpsep
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: qsepvec
      use symtypes
      integer(IL) :: CPXcopyqpsep
      integer(IL), intent(in) :: env, lpid
      real(RD), intent(in) :: qsepvec(*)
      end function CPXcopyqpsep
      END INTERFACE
!
      INTERFACE
      function CPXcopyquad(env,lpid,qmatbeg,qmatcnt,qmatind,qmatval)
!DEC$ ATTRIBUTES stdcall, alias:'CPXcopyquad' :: CPXcopyquad
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: qmatbeg, qmatcnt, qmatind, qmatval
      use symtypes
      integer(IL) :: CPXcopyquad
      integer(ILL), intent(in) :: env, lpid
      integer(IL), intent(in) :: qmatbeg(*), qmatcnt(*), qmatind(*)
      real(RD), intent(in) :: qmatval(*)
      end function CPXcopyquad
      END INTERFACE
!
      INTERFACE
      function CPXcopysos(env,lpid,numsos,numsosnz,sostype,sospri,      &
     &                    sosbeg,sosind,sosref)
!DEC$ ATTRIBUTES stdcall, alias:'CPXcopysos' :: CPXcopysos
!DEC$ ATTRIBUTES value :: env, lpid, numsos, numsosnz
!DEC$ ATTRIBUTES reference :: sostype, sospri, sosbeg, sosind, sosref
      use symtypes
      integer(IL) :: CPXcopysos
      integer(IL), intent(in) :: env, lpid, numsos, numsosnz
      integer(IL), intent(in) :: sospri(*), sosbeg(numsos),             &
     &                           sosind(numsosnz)
      real(RD), intent(in) :: sosref(numsosnz)
      character(1), intent(in) :: sostype(numsos)
      end function CPXcopysos
      END INTERFACE
!
      INTERFACE
      function CPXcopystart(env,lpid,cstat,rstat,cprim,rprim,cdual,     &
     &                      rdual)
!DEC$ ATTRIBUTES stdcall, alias:'CPXcopystart' :: CPXcopystart
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: cstat, rstat, cprim, rprim, cdual, rdual
      use symtypes
      integer(IL) :: CPXcopystart
      integer(IL), intent(in) :: env, lpid
      integer(IL), intent(in) :: cstat(*), rstat(*)
      real(RD), intent(in) :: cprim(*), rprim(*), cdual(*), rdual(*)
      end function CPXcopystart
      END INTERFACE
!
      INTERFACE
      function CPXcreateprob(env,status,probname)
!DEC$ ATTRIBUTES stdcall, alias:'CPXcreateprob' :: CPXcreateprob
!DEC$ ATTRIBUTES value :: env
!DEC$ ATTRIBUTES reference :: status, probname
      use symtypes
      integer(ILL) :: CPXcreateprob
      character(*), intent(in) :: probname
      integer(ILL), intent(in) :: env
      integer(IL), intent(inout) :: status
      end function CPXcreateprob
      END INTERFACE
!
      INTERFACE
      subroutine CPXdelchannel(env,channelpnt)
!DEC$ ATTRIBUTES stdcall, alias:'CPXdelchannel' :: CPXdelchannel
!DEC$ ATTRIBUTES value :: env
!DEC$ ATTRIBUTES reference :: channelpnt
      use symtypes
      integer(IL), intent(in) :: env
      integer(IL), intent(inout) :: channelpnt
      end subroutine CPXdelchannel
      END INTERFACE
!
      INTERFACE
      function CPXdelcols(env,lpid,begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXdelcols' :: CPXdelcols
!DEC$ ATTRIBUTES value :: env, lpid, begin, finish
      use symtypes
      integer(IL) :: CPXdelcols
      integer(IL), intent(in) :: env, lpid, begin, finish
      end function CPXdelcols
      END INTERFACE
!
      INTERFACE
      function CPXdelfpdest(env,channel,filepnt)
!DEC$ ATTRIBUTES stdcall, alias:'CPXdelfpdest' :: CPXdelfpdest
!DEC$ ATTRIBUTES value :: env, channel
!DEC$ ATTRIBUTES reference :: filepnt
      use symtypes
      integer(IL) :: CPXdelfpdest
      integer(IL), intent(in) :: env, channel
      integer(IL), intent(inout) :: filepnt
      end function CPXdelfpdest
      END INTERFACE
!
      INTERFACE
      function CPXdelrows(env,lpid,begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXdelrows' :: CPXdelrows
!DEC$ ATTRIBUTES value :: env, lpid, begin, finish
      use symtypes
      integer(IL) :: CPXdelrows
      integer(IL), intent(in) :: begin, finish
      integer(ILL), intent(in) :: env, lpid
      end function CPXdelrows
      END INTERFACE
!
      INTERFACE
      function CPXdelsetcols(env,lpid,delstat)
!DEC$ ATTRIBUTES stdcall, alias:'CPXdelsetcols' :: CPXdelsetcols
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: delstat
      use symtypes
      integer(IL) :: CPXdelsetcols
      integer(IL), intent(in) :: env, lpid
      integer(IL), intent(in) :: delstat(*)
      end function CPXdelsetcols
      END INTERFACE
!
      INTERFACE
      function CPXdelsetrows(env,lpid,delstat)
!DEC$ ATTRIBUTES stdcall, alias:'CPXdelsetrows' :: CPXdelsetrows
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: delstat
      use symtypes
      integer(IL) :: CPXdelsetrows
      integer(IL), intent(in) :: env, lpid
      integer(IL), intent(in) :: delstat(*)
      end function CPXdelsetrows
      END INTERFACE
!
      INTERFACE
      function CPXdelfuncdest(env,channel,handlepnt,funcpnt)
!DEC$ ATTRIBUTES stdcall, alias:'CPXdelfuncdest' :: CPXdelfuncdest
!DEC$ ATTRIBUTES value :: env, channel
!DEC$ ATTRIBUTES reference :: handlepnt, funcpnt
      use symtypes
      integer(IL) :: CPXdelfuncdest
      integer(IL), intent(in) :: env, channel
      integer(IL), intent(inout) :: handlepnt, funcpnt
      end function CPXdelfuncdest
      END INTERFACE
!
      INTERFACE
      subroutine CPXdisconnectchannel(env,channel)
!DEC$ ATTRIBUTES stdcall, alias:'CPXdisconnectchannel' :: CPXdisconnectchannel
!DEC$ ATTRIBUTES value :: env, channel
      use symtypes
      integer(IL), intent(in) :: env, channel
      end subroutine CPXdisconnectchannel
      END INTERFACE
!
      INTERFACE
      function CPXdisplayiis(env,lpid,channel,display)
!DEC$ ATTRIBUTES stdcall, alias:'CPXdisplayiis' :: CPXgetdisplayiis
!DEC$ ATTRIBUTES value :: env, lpid, channel, display
      use symtypes
      integer(IL) :: CPXdisplayiis
      integer(IL), intent(in) :: env, lpid, channel, display
      end function CPXdisplayiis
      END INTERFACE
!
      INTERFACE
      function CPXdperwrite(env,lpid,filename,epsilon)
!DEC$ ATTRIBUTES stdcall, alias:'CPXdperwrite' :: CPXdperwrite
!DEC$ ATTRIBUTES value :: env, lpid, epsilon
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXdperwrite
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: env, lpid
      real(RD), intent(in) :: epsilon
      end function CPXdperwrite
      END INTERFACE
!
      INTERFACE
      function CPXdualopt(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXdualopt' :: CPXdualopt
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXdualopt
      integer(ILL), intent(in) :: env, lpid
      end function CPXdualopt
      END INTERFACE
!
      INTERFACE
      function CPXdualwrite(env,lpid,filename,objshift)
!DEC$ ATTRIBUTES stdcall, alias:'CPXdualwrite' :: CPXdualwrite
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename, objshift
      use symtypes
      integer(IL) :: CPXdualwrite
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: env, lpid
      real(RD), intent(out) :: objshift
      end function CPXdualwrite
      END INTERFACE
!
      INTERFACE
      function CPXembwrite(env,lpid,filename)
!DEC$ ATTRIBUTES stdcall, alias:'CPXembwrite' :: CPXembwrite
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXembwrite
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: env, lpid
      end function CPXembwrite
      END INTERFACE
!
      INTERFACE
      function CPXfclose(filepnt)
!DEC$ ATTRIBUTES stdcall, alias:'CPXfclose' :: CPXfclose
!DEC$ ATTRIBUTES reference :: filepnt
      use symtypes
      integer(IL) :: CPXfclose
      integer(IL), intent(inout) :: filepnt
      end function CPXfclose
      END INTERFACE
!
      INTERFACE
      function CPXfindiis(env,lpid,iisnumrows,iisnumcols)
!DEC$ ATTRIBUTES stdcall, alias:'CPXfindiis' :: CPXfindiis
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: iisnumrows, iisnumcols
      use symtypes
      integer(IL) :: CPXfindiis
      integer(IL), intent(in) :: env, lpid
      integer(IL), intent(out) :: iisnumrows, iisnumcols
      end function CPXfindiis
      END INTERFACE
!
      INTERFACE
      subroutine CPXflushchannel(env,channel)
!DEC$ ATTRIBUTES stdcall, alias:'CPXflushchannel' :: CPXflushchannel
!DEC$ ATTRIBUTES value :: env, channel
      use symtypes
      integer(IL), intent(in) :: env, channel
      end subroutine CPXflushchannel
      END INTERFACE
!
      INTERFACE
      function CPXflushstdchannel(env)
!DEC$ ATTRIBUTES stdcall, alias:'CPXflushstdchannel' :: CPXflushstdchannel
!DEC$ ATTRIBUTES value :: env
      use symtypes
      integer(IL) :: CPXflushstdchannel
      integer(IL), intent(in) :: env
      end function CPXflushstdchannel
      END INTERFACE
!
      INTERFACE
      function CPXfputs(string,filepnt)
!DEC$ ATTRIBUTES stdcall, alias:'CPXfputs' :: CPXfputs
!DEC$ ATTRIBUTES reference :: string, filepnt
      use symtypes
      integer(IL) :: CPXfputs
      character(*), intent(in) :: string
      integer(IL), intent(in) :: filepnt
      end function CPXfputs
      END INTERFACE
!
      INTERFACE
      function CPXfree(memblock)
!DEC$ ATTRIBUTES stdcall, alias:'CPXfree' :: CPXfree
!DEC$ ATTRIBUTES reference :: memblock
      use symtypes
      integer(IL) :: CPXfree
      integer(IL), intent(inout) :: memblock
      end function CPXfree
      END INTERFACE
!
      INTERFACE
      function CPXfreeprob(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXfreeprob' :: CPXfreeprob
!DEC$ ATTRIBUTES value :: env
!DEC$ ATTRIBUTES reference :: lpid
      use symtypes
      integer(IL) :: CPXfreeprob
      integer(ILL), intent(in) :: env
      integer(ILL), intent(inout) :: lpid
      end function CPXfreeprob
      END INTERFACE
!
      INTERFACE
      function CPXgetbase(env,lpid,cstat,rstat)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetbase' :: CPXgetbase
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: cstat, rstat
      use symtypes
      integer(IL) :: CPXgetbase
      integer(IL), intent(in) :: env, lpid
      integer(IL), intent(out) :: cstat(*), rstat(*) 
      end function CPXgetbase
      END INTERFACE
!
      INTERFACE
      function CPXgetbestobjval(env,lpid,objval)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetbestobjval' :: CPXgetbestobjval
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: objval
      use symtypes
      integer(IL) :: CPXgetbestobjval
      integer(ILL), intent(in) :: env, lpid
      real(RD), intent(out) :: objval
      end function CPXgetbestobjval
      END INTERFACE
!
      INTERFACE
      function CPXgetcallbackinfo(env,cbdata,wherefrom,whichinfo,       &
     &                            resultpnt)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetcallbackinfo' :: CPXgetcallbackinfo
!DEC$ ATTRIBUTES value :: env, wherefrom, whichinfo
!DEC$ ATTRIBUTES reference :: cbdata, resultpnt
      use symtypes
      integer(IL) :: CPXgetcallbackinfo
      integer(IL), intent(in) :: env, cbdata, wherefrom, whichinfo
      integer(IL), intent(out) :: resultpnt
      end function CPXgetcallbackinfo
      END INTERFACE
!
      INTERFACE
      function CPXgetchannels(env,resultpnt,warningpnt,errorpnt,logpnt)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetchannels' :: CPXgetchannels
!DEC$ ATTRIBUTES value :: env
!DEC$ ATTRIBUTES reference :: resultpnt, warningpnt, errorpnt, logpnt
      use symtypes
      integer(IL) :: CPXgetchannels
      integer(IL), intent(in) :: env
      integer(IL), intent(out) :: resultpnt, warningpnt, errorpnt,      &
     &                            logpnt
      end function CPXgetchannels
      END INTERFACE
!
      INTERFACE
      function CPXgetclqcnt(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetclqcnt' :: CPXgetclqcnt
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetclqcnt
      integer(IL), intent(in) :: env, lpid
      end function CPXgetclqcnt
      END INTERFACE
!
      INTERFACE
      function CPXgetcoef(env,lpid,rowind,colind,coef)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetcoef' :: CPXgetcoef
!DEC$ ATTRIBUTES value :: env, lpid, rowind, colind
!DEC$ ATTRIBUTES reference :: coef
      use symtypes
      integer(IL) :: CPXgetcoef
      integer(IL), intent(in) :: env, lpid, rowind, colind
      real(RD), intent(out) :: coef
      end function CPXgetcoef
      END INTERFACE
!
      INTERFACE
      function CPXgetcolindex(env,lpid,colname,colindex)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetcolindex' :: CPXgetcolindex
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: colname, colindex
      use symtypes
      integer(IL) :: CPXgetcolindex
      character(*), intent(in) :: colname 
      integer(IL), intent(in) :: env, lpid
      integer(IL), intent(out) :: colindex
      end function CPXgetcolindex
      END INTERFACE
!
      INTERFACE
      function CPXgetcolname(env,lpid,nameadr,namevec,storage,surplus,  &
     &                       begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetcolname' :: CPXgetcolname
!DEC$ ATTRIBUTES value :: env, lpid, storage, begin, finish
!DEC$ ATTRIBUTES reference :: nameadr, namevec, surplus
      use symtypes
      integer(IL) :: CPXgetcolname
      character(*), intent(inout) :: namevec 
      integer(IL), intent(in) :: storage, begin, finish
      integer(ILL), intent(in) :: env, lpid
      integer(IL), intent(out) :: surplus
      integer(IL), intent(inout) :: nameadr(*)
      end function CPXgetcolname
      END INTERFACE
!
      INTERFACE
      function CPXgetcols(env,lpid,nzcnt,matbeg,matind,matval,matspace, &
     &                    surplus,begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetcols' :: CPXgetcols
!DEC$ ATTRIBUTES value :: env, lpid, matspace, begin, finish
!DEC$ ATTRIBUTES reference :: nzcnt, matbeg, matind, matval, surplus
      use symtypes
      integer(IL) :: CPXgetcols
      integer(IL), intent(in) :: env, lpid, matspace, begin, finish
      integer(IL), intent(out) :: nzcnt, surplus
      integer(IL), intent(out) :: matbeg(finish - begin + 1)
      integer(IL), intent(out) :: matind(*)
      real(RD), intent(out) :: matval(*)
      end function CPXgetcols
      END INTERFACE
!
      INTERFACE
      function CPXgetcovcnt(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetcovcnt' :: CPXgetcovcnt
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetcovcnt
      integer(IL), intent(in) :: env, lpid
      end function CPXgetcovcnt
      END INTERFACE
!
      INTERFACE
      function CPXgetcrossdexchcnt(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetcrossdexchcnt' :: CPXgetcrossdexchcnt
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetcrossdexchcnt
      integer(IL), intent(in) :: env, lpid
      end function CPXgetcrossdexchcnt
      END INTERFACE
!
      INTERFACE
      function CPXgetcrossdpushcnt(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetcrossdpushcnt' :: CPXgetcrossdpushcnt
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetcrossdpushcnt
      integer(IL), intent(in) :: env, lpid
      end function CPXgetcrossdpushcnt
      END INTERFACE
!
      INTERFACE
      function CPXgetcrosspexchcnt(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetcrosspexchcnt' :: CPXgetcrosspexchcnt
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetcrosspexchcnt
      integer(IL), intent(in) :: env, lpid
      end function CPXgetcrosspexchcnt
      END INTERFACE
!
      INTERFACE
      function CPXgetcrossppushcnt(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetcrossppushcnt' :: CPXgetcrossppushcnt
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetcrossppushcnt
      integer(IL), intent(in) :: env, lpid
      end function CPXgetcrossppushcnt
      END INTERFACE
!
      INTERFACE
      function CPXgetctype(env,lpid,ctype,begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetctype' :: CPXgetctype
!DEC$ ATTRIBUTES value :: env, lpid, begin, finish
!DEC$ ATTRIBUTES reference :: ctype
      use symtypes
      integer(IL) :: CPXgetctype
      integer(IL), intent(in) :: env, lpid, begin, finish
      character(1), intent(out) :: ctype(finish - begin + 1)
      end function CPXgetctype
      END INTERFACE
!
      INTERFACE
      function CPXgetcutoff(env,lpid,cutoff)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetcutoff' :: CPXgetcutoff
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: cutoff
      use symtypes
      integer(IL) :: CPXgetcutoff
      integer(IL), intent(in) :: env, lpid
      real(RD), intent(out) :: cutoff
      end function CPXgetcutoff
      END INTERFACE
!
      INTERFACE
      function CPXgetdblparam(env,whichparam,value)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetdblparam' :: CPXgetdblparam
!DEC$ ATTRIBUTES value :: env, whichparam
!DEC$ ATTRIBUTES reference :: value
      use symtypes
      integer(IL) :: CPXgetdblparam
      integer(IL), intent(in) :: env, whichparam
      real(RD), intent(out) :: value
      end function CPXgetdblparam
      END INTERFACE
!
      INTERFACE
      function CPXgetdj(env,lpid,dj,begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetdj' :: CPXgetdj
!DEC$ ATTRIBUTES value :: env, lpid, begin, finish
!DEC$ ATTRIBUTES reference :: dj
      use symtypes
      integer(IL) :: CPXgetdj
      integer(IL), intent(in) :: env, lpid, begin, finish
      real(RD), intent(out) :: dj(finish - begin + 1) 
      end function CPXgetdj
      END INTERFACE
!
      INTERFACE
      subroutine CPXgeterrorstring(env,errcode,errmsg)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgeterrorstring' :: CPXgeterrorstring
!DEC$ ATTRIBUTES value :: env, errcode
!DEC$ ATTRIBUTES reference :: errmsg
      use symtypes
      character(*), intent(inout) :: errmsg
      integer(IL), intent(in) :: env, errcode
      end subroutine CPXgeterrorstring
      END INTERFACE
!
      INTERFACE
      function CPXgetgenclqcnt(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetgenclqcnt' :: CPXgetgenclqcnt
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetgenclqcnt
      integer(IL), intent(in) :: env, lpid
      end function CPXgetgenclqcnt
      END INTERFACE
!
      INTERFACE
      function CPXgetgrad(env,lpid,colind,head,colcoe)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetgrad' :: CPXgetgrad
!DEC$ ATTRIBUTES value :: env, lpid, colind
!DEC$ ATTRIBUTES reference :: head, colcoe
      use symtypes
      integer(IL) :: CPXgetgrad
      integer(IL), intent(in) :: env, lpid, colind
      integer(IL), intent(out) :: head(*)
      real(RD), intent(out) :: colcoe(*)
      end function CPXgetgrad
      END INTERFACE
!
      INTERFACE
      function CPXgetiis(env,lpid,iisstat,rowind,rowbdstat,iisnumrows,  &
     &                   colind,colbdstat,iisnumcols)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetiis' :: CPXgetiis
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: iisstat, rowind, rowbdstat,iisnumrows
!DEC$ ATTRIBUTES reference :: colind, colbdstat, iisnumcols
      use symtypes
      integer(IL) :: CPXgetiis
      integer(IL), intent(in) :: env, lpid
      integer(IL), intent(out) :: iisstat, iisnumrows, iisnumcols
      integer(IL), intent(out) :: rowind(*), rowbdstat(*), colind(*),   &
     &                            colbdstat(*)
      end function CPXgetiis
      END INTERFACE
!
      INTERFACE
      function CPXgetintparam(env,whichparam,newvalue)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetintparam' :: CPXgetintparam
!DEC$ ATTRIBUTES value :: env, whichparam
!DEC$ ATTRIBUTES reference :: newvalue
      use symtypes
      integer(IL) :: CPXgetintparam
      integer(IL), intent(in) :: env, whichparam
      integer(IL), intent(out) :: newvalue
      end function CPXgetintparam
      END INTERFACE
!
      INTERFACE
      function CPXgetitcnt(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetitcnt' :: CPXgetitcnt
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetitcnt
      integer(IL), intent(in) :: env, lpid
      end function CPXgetitcnt
      END INTERFACE
!
      INTERFACE
      function CPXgetlb(env,lpid,lobnds,begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetlb' :: CPXgetlb
!DEC$ ATTRIBUTES value :: env, lpid, begin, finish
!DEC$ ATTRIBUTES reference :: lobnds
      use symtypes
      integer(IL) :: CPXgetlb
      integer(IL), intent(in) :: env, lpid, begin, finish
      real(RD), intent(out) :: lobnds(finish - begin + 1)
      end function CPXgetlb
      END INTERFACE
!
      INTERFACE
      function CPXgetlogfile(env,filepntloc)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetlogfile' :: CPXgetlogfile
!DEC$ ATTRIBUTES value :: env
!DEC$ ATTRIBUTES reference :: filepntloc
      use symtypes
      integer(IL) :: CPXgetlogfile
      integer(ILL), intent(in) :: env
      integer(IL), intent(out) :: filepntloc
      end function CPXgetlogfile
      END INTERFACE
!
      INTERFACE
      function CPXgetlpcallbackfunc(env,adrfuncpnt,adrprivpnt)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetlpcallbackfunc' :: CPXgetlpcallbackfunc
!DEC$ ATTRIBUTES value :: env
!DEC$ ATTRIBUTES reference :: adrfuncpnt, adrprivpnt
      use symtypes
      integer(IL) :: CPXgetlpcallbackfunc
      integer(IL), intent(in) :: env
      integer(IL), intent(out) :: adrfuncpnt, adrprivpnt
      end function CPXgetlpcallbackfunc
      END INTERFACE
!
      INTERFACE
      function CPXgetmethod(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetmethod' :: CPXgetmethod
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetmethod
      integer(IL), intent(in) :: env, lpid
      end function CPXgetmethod
      END INTERFACE
!
      INTERFACE
      function CPXgetmipcallbackfunc(env,adrfuncpnt,adrprivpnt)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetmipcallbackfunc' :: CPXgetmipcallbackfunc
!DEC$ ATTRIBUTES value :: env
!DEC$ ATTRIBUTES reference :: adrfuncpnt, adrprivpnt
      use symtypes
      integer(IL) :: CPXgetmipcallbackfunc
      integer(IL), intent(in) :: env
      integer(IL), intent(out) :: adrfuncpnt, adrprivpnt
      end function CPXgetmipcallbackfunc
      END INTERFACE
!
      INTERFACE
      function CPXgetmipitcnt(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetmipitcnt' :: CPXgetmipitcnt
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetmipitcnt
      integer(IL), intent(in) :: env, lpid
      end function CPXgetmipitcnt
      END INTERFACE
!
      INTERFACE
      function CPXgetmipobjval(env,lpid,objval)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetmipobjval' :: CPXgetmipobjval
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: objval
      use symtypes
      integer(IL) :: CPXgetmipobjval
      integer(IL), intent(in) :: env, lpid
      real(RD), intent(out) :: objval
      end function CPXgetmipobjval
      END INTERFACE
!
      INTERFACE
      function CPXgetmipslack(env,lpid,slack,begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetmipslack' :: CPXgetmipslack
!DEC$ ATTRIBUTES value :: env, lpid, begin, finish
!DEC$ ATTRIBUTES reference :: slack
      use symtypes
      integer(IL) :: CPXgetmipslack
      integer(IL), intent(in) :: env, lpid, begin, finish
      real(RD), intent(out) :: slack(finish - begin + 1)
      end function CPXgetmipslack
      END INTERFACE
!
      INTERFACE
      function CPXgetmipstart(env,lpid,cnt,indices,values,startspace,   &
     &                        surplus)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetmipstart' :: CPXgetmipstart
!DEC$ ATTRIBUTES value :: env, lpid, startspace
!DEC$ ATTRIBUTES reference :: cnt, indices, values, surplus
      use symtypes
      integer(IL) :: CPXgetmipstart
      integer(IL), intent(in) :: env, lpid, startspace
      integer(IL), intent(out) :: cnt, surplus
      integer(IL), intent(out) :: indices(*), values(*)
      end function CPXgetmipstart
      END INTERFACE
!
      INTERFACE
      function CPXgetmipx(env,lpid,x,begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetmipx' :: CPXgetmipx
!DEC$ ATTRIBUTES value :: env, lpid, begin, finish
!DEC$ ATTRIBUTES reference :: x
      use symtypes
      integer(IL) :: CPXgetmipx
      integer(IL), intent(in) :: env, lpid, begin, finish
      real(RD), intent(out) :: x(finish - begin + 1)
      end function CPXgetmipx
      END INTERFACE
!
      INTERFACE
      function CPXgetnodecnt(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetnodecnt' :: CPXgetnodecnt
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetnodecnt
      integer(IL), intent(in) :: env, lpid
      end function CPXgetnodecnt
      END INTERFACE
!
      INTERFACE
      function CPXgetnodeint(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetnodeint' :: CPXgetnodeint
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetnodeint
      integer(IL), intent(in) :: env, lpid
      end function CPXgetnodeint
      END INTERFACE
!
      INTERFACE
      function CPXgetnodeleftcnt(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetnodeleftcnt' :: CPXgetnodeleftcnt
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetnodeleftcnt
      integer(IL), intent(in) :: env, lpid
      end function CPXgetnodeleftcnt
      END INTERFACE
!
      INTERFACE
      function CPXgetnumbin(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetnumbin' :: CPXgetnumbin
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetnumbin
      integer(IL), intent(in) :: env, lpid
      end function CPXgetnumbin
      END INTERFACE
!
      INTERFACE
      function CPXgetnumcols(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetnumcols' :: CPXgetnumcols
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetnumcols
      integer(IL), intent(in) :: env, lpid
      end function CPXgetnumcols
      END INTERFACE
!
      INTERFACE
      function CPXgetnumint(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetnumint' :: CPXgetnumint
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetnumint
      integer(IL), intent(in) :: env, lpid
      end function CPXgetnumint
      END INTERFACE
!
      INTERFACE
      function CPXgetnumnz(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetnumnz' :: CPXgetnumnz
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetnumnz
      integer(IL), intent(in) :: env, lpid
      end function CPXgetnumnz
      END INTERFACE
!
      INTERFACE
      function CPXgetnumqpnz(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetnumqpnz' :: CPXgetnumqpnz
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetnumqpnz
      integer(IL), intent(in) :: env, lpid
      end function CPXgetnumqpnz
      END INTERFACE
!
      INTERFACE
      function CPXgetnumquad(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetnumquad' :: CPXgetnumquad
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetnumquad
      integer(IL), intent(in) :: env, lpid
      end function CPXgetnumquad
      END INTERFACE
!
      INTERFACE
      function CPXgetnumrows(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetnumrows' :: CPXgetnumrows
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetnumrows
      integer(IL), intent(in) :: env, lpid
      end function CPXgetnumrows
      END INTERFACE
!
      INTERFACE
      function CPXgetnumsos(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetnumsos' :: CPXgetnumsos
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetnumsos
      integer(IL), intent(in) :: env, lpid
      end function CPXgetnumsos
      END INTERFACE
!
      INTERFACE
      function CPXgetobj(env,lpid,objcoe,begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetobj' :: CPXgetobj
!DEC$ ATTRIBUTES value :: env, lpid, begin, finish
!DEC$ ATTRIBUTES reference :: objcoe
      use symtypes
      integer(IL) :: CPXgetobj
      integer(IL), intent(in) :: env, lpid, begin, finish
      real(RD), intent(out) :: objcoe(finish - begin + 1)
      end function CPXgetobj
      END INTERFACE
!
      INTERFACE
      function CPXgetobjname(env,lpid,name,lenth,surplus)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetobjname' :: CPXgetobjname
!DEC$ ATTRIBUTES value :: env, lpid, lenth
!DEC$ ATTRIBUTES reference :: name, surplus
      use symtypes
      integer(IL) :: CPXgetobjname
      integer(IL), intent(in) :: env, lpid, lenth
      integer(IL), intent(out) :: surplus
      character(lenth), intent(out) :: name 
      end function CPXgetobjname
      END INTERFACE
!
      INTERFACE
      function CPXgetobjsen(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetobjsen' :: CPXgetobjsen
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetobjsen
      integer(IL), intent(in) :: env, lpid
      end function CPXgetobjsen
      END INTERFACE
!
      INTERFACE
      function CPXgetobjval(env,lpid,objval)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetobjval' :: CPXgetobjval
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: objval
      use symtypes
      integer(IL) :: CPXgetobjval
      integer(ILL), intent(in) :: env, lpid
      real(RD), intent(out) :: objval
      end function CPXgetobjval
      END INTERFACE
!
      INTERFACE
      function CPXgetorder(env,lpid,cnt,indices,priority,direction,     &
     &                     ordspace,surplus)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetorder' :: CPXgetorder
!DEC$ ATTRIBUTES value :: env, lpid, ordspace
!DEC$ ATTRIBUTES reference :: cnt, indices, priority, direction, surplus
      use symtypes
      integer(IL) :: CPXgetorder
      integer(IL), intent(in) :: env, lpid, ordspace
      integer(IL), intent(out) :: cnt, surplus
      integer(IL), intent(out) :: indices(*), priority(*), direction(*)
      end function CPXgetorder
      END INTERFACE
!
      INTERFACE
      function CPXgetphase1cnt(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetphase1cnt' :: CPXgetphase1cnt
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetphase1cnt
      integer(IL), intent(in) :: env, lpid
      end function CPXgetphase1cnt
      END INTERFACE
!
      INTERFACE
      function CPXgetpi(env,lpid,pi,begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetpi' :: CPXgetpi
!DEC$ ATTRIBUTES value :: env, lpid, begin, finish
!DEC$ ATTRIBUTES reference :: pi
      use symtypes
      integer(IL) :: CPXgetpi
      integer(IL), intent(in) :: env, lpid, begin, finish
      real(RD), intent(out) :: pi(finish - begin + 1) 
      end function CPXgetpi
      END INTERFACE
!
      INTERFACE
      function CPXgetprobname(env,lpid,name,lenth,surplus)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetprobname' :: CPXgetprobname
!DEC$ ATTRIBUTES value :: env, lpid, lenth
!DEC$ ATTRIBUTES reference :: name, surplus
      use symtypes
      integer(IL) :: CPXgetprobname
      integer(IL), intent(in) :: env, lpid, lenth
      integer(IL), intent(out) :: surplus
      character(lenth), intent(out) :: name 
      end function CPXgetprobname
      END INTERFACE
!
      INTERFACE
      function CPXgetprobtype(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetprobtype' :: CPXgetprobtype
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetprobtype
      integer(IL), intent(in) :: env, lpid
      end function CPXgetprobtype
      END INTERFACE
!
      INTERFACE
      function CPXgetqpcoef(env,lpid,rownum,colnum,coef)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetqpcoef' :: CPXgetqpcoef
!DEC$ ATTRIBUTES value :: env, lpid, rownum, colnum
!DEC$ ATTRIBUTES reference :: coef
      use symtypes
      integer(IL) :: CPXgetqpcoef
      integer(IL), intent(in) :: env, lpid, rownum, colnum
      real(RD), intent(out) :: coef
      end function CPXgetqpcoef
      END INTERFACE
!
      INTERFACE
      function CPXgetrhs(env,lpid,rhsval,begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetrhs' :: CPXgetrhs
!DEC$ ATTRIBUTES value :: env, lpid, begin, finish
!DEC$ ATTRIBUTES reference :: rhsval
      use symtypes
      integer(IL) :: CPXgetrhs
      integer(IL), intent(in) :: env, lpid, begin, finish
      real(RD), intent(out) :: rhsval(finish - begin + 1)
      end function CPXgetrhs
      END INTERFACE
!
      INTERFACE
      function CPXgetrngval(env,lpid,rngval,begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetrngval' :: CPXgetrngval
!DEC$ ATTRIBUTES value :: env, lpid, begin, finish
!DEC$ ATTRIBUTES reference :: rngval
      use symtypes
      integer(IL) :: CPXgetrngval
      integer(IL), intent(in) :: env, lpid, begin, finish
      real(RD), intent(out) :: rngval(finish - begin + 1) 
      end function CPXgetrngval
      END INTERFACE
!
      INTERFACE
      function CPXgetrowindex(env,lpid,rowname,rowindex)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetrowindex' :: CPXgetrowindex
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: rowname, rowindex
      use symtypes
      integer(IL) :: CPXgetrowindex
      character(*), intent(in) :: rowname 
      integer(IL), intent(in) :: env, lpid
      integer(IL), intent(out) :: rowindex
      end function CPXgetrowindex
      END INTERFACE
!
      INTERFACE
      function CPXgetrowname(env,lpid,nameadr,namevec,storage,surplus,  &
     &                       begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetrowname' :: CPXgetrowname
!DEC$ ATTRIBUTES value :: env, lpid, storage, begin, finish
!DEC$ ATTRIBUTES reference :: nameadr, namevec, surplus
      use symtypes
      integer(IL) :: CPXgetrowname
      character(*), intent(inout) :: namevec 
      integer(IL), intent(in) :: env, lpid, storage, begin, finish
      integer(IL), intent(out) :: surplus
      integer(IL), intent(inout) :: nameadr(*)
      end function CPXgetrowname
      END INTERFACE
!
      INTERFACE
      function CPXgetrows(env,lpid,nzcnt,matbeg,matind,matval,matspace, &
     &                    surplus,begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetrows' :: CPXgetrows
!DEC$ ATTRIBUTES value :: env, lpid, matspace, begin, finish
!DEC$ ATTRIBUTES reference :: nzcnt, matbeg, matind, matval, surplus
      use symtypes
      integer(IL) :: CPXgetrows
      integer(IL), intent(in) :: env, lpid, matspace, begin, finish
      integer(IL), intent(out) :: nzcnt, surplus
      integer(IL), intent(out) :: matbeg(finish - begin + 1)
      integer(IL), intent(out) :: matind(*)
      real(RD), intent(out) :: matval(*)
      end function CPXgetrows
      END INTERFACE
!
      INTERFACE
      function CPXgetsbcnt(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetsbcnt' :: CPXgetsbcnt
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetsbcnt
      integer(IL), intent(in) :: env, lpid
      end function CPXgetsbcnt
      END INTERFACE
!
      INTERFACE
      function CPXgetsense(env,lpid,sense,begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetsense' :: CPXgetsense
!DEC$ ATTRIBUTES value :: env, lpid, begin, finish
!DEC$ ATTRIBUTES reference :: sense
      use symtypes
      integer(IL) :: CPXgetsense
      integer(IL), intent(in) :: env, lpid, begin, finish
      character(1), intent(out) :: sense(finish - begin + 1)
      end function CPXgetsense
      END INTERFACE
!
      INTERFACE
      function CPXgetslack(env,lpid,slack,begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetslack' :: CPXgetslack
!DEC$ ATTRIBUTES value :: env, lpid, begin, finish
!DEC$ ATTRIBUTES reference :: slack
      use symtypes
      integer(IL) :: CPXgetslack
      integer(IL), intent(in) :: env, lpid, begin, finish
      real(RD), intent(out) :: slack(finish - begin + 1) 
      end function CPXgetslack
      END INTERFACE
!
      INTERFACE
      function CPXgetsos(env,lpid,numsosnz,sostype,sospri,sosbeg,sosind,&
     &                   sosref,sosspace,surplus,begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetsos' :: CPXgetsos
!DEC$ ATTRIBUTES value :: env, lpid, sosspace, begin, finish
!DEC$ ATTRIBUTES reference :: numsosnz, sostype, sospri, sosbeg, sosind
!DEC$ ATTRIBUTES reference :: sosref, surplus
      use symtypes
      integer(IL) :: CPXgetsos
      integer(IL), intent(in) :: env, lpid, sosspace, begin, finish
      integer(IL), intent(out) :: numsosnz, surplus
      integer(IL), intent(out) :: sosbeg(finish - begin + 1)
      integer(IL), intent(out) :: sospri(*), sosind(*)
      real(RD), intent(out) :: sosref(*)
      character(1), intent(out) :: sostype(finish - begin + 1)
      end function CPXgetsos
      END INTERFACE
!
      INTERFACE
      function CPXgetstat(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetstat' :: CPXgetstat
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetstat
      integer(ILL), intent(in) :: env, lpid
      end function CPXgetstat
      END INTERFACE
!
      INTERFACE
      function CPXgetstrparam(env,whichparam,value)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetstrparam' :: CPXgetstrparam
!DEC$ ATTRIBUTES value :: env, whichparam
!DEC$ ATTRIBUTES reference :: value
      use symtypes
      integer(IL) :: CPXgetstrparam
      character(*), intent(out) :: value
      integer(IL), intent(in) :: env, whichparam
      end function CPXgetstrparam
      END INTERFACE
!
      INTERFACE
      function CPXgetsubstat(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetsubstat' :: CPXgetsubstat
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXgetsubstat
      integer(IL), intent(in) :: env, lpid
      end function CPXgetsubstat
      END INTERFACE
!
      INTERFACE
      function CPXgetub(env,lpid,upbnds,begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetub' :: CPXgetub
!DEC$ ATTRIBUTES value :: env, lpid, begin, finish
!DEC$ ATTRIBUTES reference :: upbnds
      use symtypes
      integer(IL) :: CPXgetub
      integer(IL), intent(in) :: env, lpid, begin, finish
      real(RD), intent(out) :: upbnds(finish - begin + 1)
      end function CPXgetub
      END INTERFACE
!
      INTERFACE
      function CPXgetx(env,lpid,x,begin,finish)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetx' :: CPXgetx
!DEC$ ATTRIBUTES value :: env, lpid, begin, finish
!DEC$ ATTRIBUTES reference :: x
      use symtypes
      integer(IL) :: CPXgetx
      integer(IL), intent(in) :: begin, finish
      integer(ILL), intent(in) :: env, lpid
      real(RD), intent(out) :: x(finish - begin + 1) 
      end function CPXgetx
      END INTERFACE
!
      INTERFACE
      function CPXhybbaropt(env,lpid,method)
!DEC$ ATTRIBUTES stdcall, alias:'CPXhybbaropt' :: CPXhybbaropt
!DEC$ ATTRIBUTES value :: env, lpid, method
      use symtypes
      integer(IL) :: CPXhybbaropt
      character(1), intent(in) :: method
      integer(IL), intent(in) :: env, lpid
      end function CPXhybbaropt
      END INTERFACE
!
      INTERFACE
      function CPXhybnetopt(env,lpid,method)
!DEC$ ATTRIBUTES stdcall, alias:'CPXhybnetopt' :: CPXhybnetopt
!DEC$ ATTRIBUTES value :: env, lpid, method
      use symtypes
      integer(IL) :: CPXhybnetopt
      character(1), intent(in) :: method
      integer(IL), intent(in) :: env, lpid
      end function CPXhybnetopt
      END INTERFACE
!
      INTERFACE
      function CPXiiswrite(env,lpid,filename)
!DEC$ ATTRIBUTES stdcall, alias:'CPXiiswrite' :: CPXiiswrite
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXiiswrite
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: env, lpid
      end function CPXiiswrite
      END INTERFACE
!
      INTERFACE
      function CPXinfodblparam(env,whichparam,defvalue,minvalue,        &
     &                         maxvalue)
!DEC$ ATTRIBUTES stdcall, alias:'CPXinfodblparam' :: CPXinfodblparam
!DEC$ ATTRIBUTES value :: env, whichparam
!DEC$ ATTRIBUTES reference :: defvalue, minvalue, maxvalue
      use symtypes
      integer(IL) :: CPXinfodblparam
      integer(IL), intent(in) :: env, whichparam
      real(RD), intent(out) :: defvalue, minvalue, maxvalue
      end function CPXinfodblparam
      END INTERFACE
!
      INTERFACE
      function CPXinfointparam(env,whichparam,defvalue,minvalue,        &
     &                         maxvalue)
!DEC$ ATTRIBUTES stdcall, alias:'CPXinfointparam' :: CPXinfointparam
!DEC$ ATTRIBUTES value :: env, whichparam
!DEC$ ATTRIBUTES reference :: defvalue, minvalue, maxvalue
      use symtypes
      integer(IL) :: CPXinfointparam
      integer(IL), intent(in) :: env, whichparam
      integer(IL), intent(out) :: defvalue, minvalue, maxvalue
      end function CPXinfointparam
      END INTERFACE
!
      INTERFACE
      function CPXinfostrparam(env,whichparam,value)
!DEC$ ATTRIBUTES stdcall, alias:'CPXinfostrparam' :: CPXgetinfoparam
!DEC$ ATTRIBUTES value :: env, whichparam
!DEC$ ATTRIBUTES reference :: value
      use symtypes
      integer(IL) :: CPXinfostrparam
      character(*), intent(out) :: value
      integer(IL), intent(in) :: env, whichparam
      end function CPXinfostrparam
      END INTERFACE
!
      INTERFACE
      function CPXmalloc(numbytes)
!DEC$ ATTRIBUTES stdcall, alias:'CPXmalloc' :: CPXmalloc
!DEC$ ATTRIBUTES value :: numbytes
      use symtypes
      integer(IL) :: CPXmalloc
      integer(IL), intent(in) :: numbytes
      end function CPXmalloc
      END INTERFACE
!
      INTERFACE
      function CPXmbasewrite(env,lpid,filename)
!DEC$ ATTRIBUTES stdcall, alias:'CPXmbasewrite' :: CPXmbasewrite
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXmbasewrite
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: env, lpid
      end function CPXmbasewrite
      END INTERFACE
!
      INTERFACE
      function CPXmemcpy(topnt,frompnt,numbytes)
!DEC$ ATTRIBUTES stdcall, alias:'CPXmemcpy' :: CPXmemcpy
!DEC$ ATTRIBUTES value :: numbytes
!DEC$ ATTRIBUTES reference :: topnt, frompnt
      use symtypes
      integer(IL) :: CPXmemcpy
      integer(IL), intent(in) :: frompnt, numbytes
      integer(IL), intent(out) :: topnt
      end function CPXmemcpy
      END INTERFACE
!
      INTERFACE
      function CPXmipopt(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXmipopt' :: CPXmipopt
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXmipopt
      integer(ILL), intent(in) :: env, lpid
      end function CPXmipopt
      END INTERFACE
!
      INTERFACE
      function CPXmsg(channel,fmtstr)
!DEC$ ATTRIBUTES C, varying, alias:'CPXmsg' :: CPXmsg
!DEC$ ATTRIBUTES value :: channel
!DEC$ ATTRIBUTES reference :: fmtstr
      use symtypes
      integer(IL) :: CPXmsg
      character(*), intent(in) :: fmtstr
      integer(IL), intent(in) :: channel
      end function CPXmsg
      END INTERFACE
!
      INTERFACE
      function CPXmsgstr(channel,message)
!DEC$ ATTRIBUTES stdcall, alias:'CPXmsgstr' :: CPXmsgstr
!DEC$ ATTRIBUTES reference :: channel, message
      use symtypes
      integer(IL) :: CPXmsgstr
      character(*), intent(in) :: message
      integer(IL), intent(inout) :: channel
      end function CPXmsgstr
      END INTERFACE
!
      INTERFACE
      function CPXnetopt(env,lpid,netstatus,numnodes,numarcs,itcnt)
!DEC$ ATTRIBUTES stdcall, alias:'CPXnetopt' :: CPXnetopt
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: netstatus, numnodes, numarcs, itcnt
      use symtypes
      integer(IL) :: CPXnetopt
      integer(IL), intent(in) :: env, lpid
      integer(IL), intent(out) :: netstatus, numnodes, numarcs, itcnt
      end function CPXnetopt
      END INTERFACE
!
      INTERFACE
      function CPXnewcols(env,lpid,ccnt,objcoe,lobnds,upbnds,ctype,     &
     &                    colnameadr)
!DEC$ ATTRIBUTES stdcall, alias:'CPXnewcols' :: CPXnewcols
!DEC$ ATTRIBUTES value :: env, lpid, ccnt
!DEC$ ATTRIBUTES reference :: objcoe, lobnds, upbnds, ctype, colnameadr
      use symtypes
      integer(IL) :: CPXnewcols
      integer(IL), intent(in) :: ccnt
      integer(ILL), intent(in) :: lpid
      integer(ILL), intent(in) :: env
      integer(IL), intent(in) :: colnameadr(*)
      real(RD), intent(in) :: objcoe(*), lobnds(*), upbnds(*)
      character(1), intent(in) :: ctype(*)
      end function CPXnewcols
      END INTERFACE
!
      INTERFACE
      function CPXnewrows(env,lpid,rcnt,rhsval,sense,rngval,rownameadr)
!DEC$ ATTRIBUTES stdcall, alias:'CPXnewrows' :: CPXnewrows
!DEC$ ATTRIBUTES value :: env, lpid, rcnt
!DEC$ ATTRIBUTES reference :: rhsval, sense, rngval, rownameadr
      use symtypes
      integer(IL) :: CPXnewrows
      integer(IL), intent(in) :: env, lpid, rcnt
      integer(IL), intent(in) :: rownameadr(*)
      real(RD), intent(in) :: rhsval(*), rngval(*)
      character(1), intent(in) :: sense(*)
      end function CPXnewrows
      END INTERFACE
!
      INTERFACE
      function CPXobjsa(env,lpid,begin,finish,lower,upper)
!DEC$ ATTRIBUTES stdcall, alias:'CPXobjsa' :: CPXobjsa
!DEC$ ATTRIBUTES value :: env, lpid, begin, finish
!DEC$ ATTRIBUTES reference :: lower, upper
      use symtypes
      integer(IL) :: CPXobjsa
      integer(IL), intent(in) :: env, lpid, begin, finish
      real(RD), intent(out) :: lower(finish - begin + 1),               &
     &                         upper(finish - begin + 1)
      end function CPXobjsa
      END INTERFACE
!
      INTERFACE
      function CPXopenCPLEX(status)
!DEC$ ATTRIBUTES stdcall, alias:'CPXopenCPLEX' :: CPXopenCPLEX
!DEC$ ATTRIBUTES reference :: status
      use symtypes
      integer(ILL) :: CPXopenCPLEX
      integer(ILL), intent(inout) :: status
      end function CPXopenCPLEX
      END INTERFACE
!
      INTERFACE
      function CPXopenCPLEXruntime(status,serialnum,licenvstring)
!DEC$ ATTRIBUTES stdcall, alias:'CPXopenCPLEXruntime' :: CPXopenCPLEXruntime
!DEC$ ATTRIBUTES value :: serialnum
!DEC$ ATTRIBUTES reference :: status, licenvstring
      use symtypes
      integer(IL) :: CPXopenCPLEXruntime
      character(*), intent(in) :: licenvstring
      integer(IL), intent(in) :: serialnum
      integer(IL), intent(out) :: status
      end function CPXopenCPLEXruntime
      END INTERFACE
!
      INTERFACE
      function CPXordwrite(env,lpid,filename)
!DEC$ ATTRIBUTES stdcall, alias:'CPXordwrite' :: CPXordwrite
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXordwrite
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: env, lpid
      end function CPXordwrite
      END INTERFACE
!
      INTERFACE
      function CPXpperwrite(env,lpid,filename,epsilon)
!DEC$ ATTRIBUTES stdcall, alias:'CPXpperwrite' :: CPXpperwrite
!DEC$ ATTRIBUTES value :: env, lpid, epsilon
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXpperwrite
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: env, lpid
      real(RD), intent(in) :: epsilon
      end function CPXpperwrite
      END INTERFACE
!
      INTERFACE
      function CPXpreslvwrite(env,lpid,filename,objoff)
!DEC$ ATTRIBUTES stdcall, alias:'CPXpreslvwrite' :: CPXpreslvwrite
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename, objoff
      use symtypes
      integer(IL) :: CPXpreslvwrite
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: env, lpid
      real(RD), intent(out) :: objoff
      end function CPXpreslvwrite
      END INTERFACE
!
      INTERFACE
      function CPXprimopt(env,lpid)
!DEC$ ATTRIBUTES stdcall, alias:'CPXprimopt' :: CPXprimopt
!DEC$ ATTRIBUTES value :: env, lpid
      use symtypes
      integer(IL) :: CPXprimopt
      integer(ILL), intent(in) :: env, lpid
      end function CPXprimopt
      END INTERFACE
!
      INTERFACE
      function CPXqpwrite(env,lpid,filename)
!DEC$ ATTRIBUTES stdcall, alias:'CPXqpwrite' :: CPXqpwrite
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXqpwrite
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: env, lpid
      end function CPXqpwrite
      END INTERFACE
!
      INTERFACE
      function CPXreadcopybase(env,lpid,filename)
!DEC$ ATTRIBUTES stdcall, alias:'CPXreadcopybase' :: CPXreadcopybase
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXreadcopybase
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: env, lpid
      end function CPXreadcopybase
      END INTERFACE
!
      INTERFACE
      function CPXreadcopyorder(env,lpid,filename)
!DEC$ ATTRIBUTES stdcall, alias:'CPXreadcopyorder' :: CPXreadcopyorder
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXreadcopyorder
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: env, lpid
      end function CPXreadcopyorder
      END INTERFACE
!
      INTERFACE
      function CPXreadcopyprob(env,lpid,filename,filetype)
!DEC$ ATTRIBUTES stdcall, alias:'CPXreadcopyprob' :: CPXreadcopyprob
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename, filetype
      use symtypes
      integer(IL) :: CPXreadcopyprob
      character(*), intent(in) :: filename, filetype
      integer(IL), intent(in) :: env, lpid
      end function CPXreadcopyprob
      END INTERFACE
!
      INTERFACE
      function CPXreadcopyqp(env,lpid,filename)
!DEC$ ATTRIBUTES stdcall, alias:'CPXreadcopyqp' :: CPXreadcopyqp
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXreadcopyqp
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: env, lpid
      end function CPXreadcopyqp
      END INTERFACE
!
      INTERFACE
      function CPXreadcopysol(env,lpid,filename)
!DEC$ ATTRIBUTES stdcall, alias:'CPXreadcopysol' :: CPXreadcopysol
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXreadcopysos
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: env, lpid
      end function CPXreadcopysol
      END INTERFACE
!
      INTERFACE
      function CPXreadcopymipstarts(env,lpid,filename)
!DEC$ ATTRIBUTES stdcall, alias:'CPXreadcopymipstarts' :: CPXreadcopymipstarts
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXreadcopymipstarts
      character(*), intent(in) :: filename
      integer(ILL), intent(in) :: env, lpid
      end function CPXreadcopymipstarts
      END INTERFACE
!
      INTERFACE
      function CPXreadcopytree(env,lpid,filename)
!DEC$ ATTRIBUTES stdcall, alias:'CPXreadcopytree' :: CPXreadcopytree
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXreadcopytree
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: env, lpid
      end function CPXreadcopytree
      END INTERFACE
!
      INTERFACE
      function CPXreadcopyvec(env,lpid,filename)
!DEC$ ATTRIBUTES stdcall, alias:'CPXreadcopyvec' :: CPXreadcopyvec
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXreadcopyvec
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: env, lpid
      end function CPXreadcopyvec
      END INTERFACE
!
      INTERFACE
      function CPXrealloc(memloc,numbytes)
!DEC$ ATTRIBUTES stdcall, alias:'CPXrealloc' :: CPXrealloc
!DEC$ ATTRIBUTES value :: numbytes
!DEC$ ATTRIBUTES reference :: memloc
      use symtypes
      integer(IL) :: CPXrealloc
      integer(IL), intent(in) :: memloc, numbytes
      end function CPXrealloc
      END INTERFACE

      INTERFACE
      function CPXrefineconflict(env,lpid,confnumrows_p, confnumcols_p)
!DEC$ ATTRIBUTES stdcall, alias:'CPXrefineconflict' :: CPXrefineconflict
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES value :: confnumrows_p, confnumcols_p
      use symtypes
      integer(IL) :: CPXrefineconflict
      integer(ILL), intent(in) :: env, lpid
      integer(IL), intent(in) :: confnumrows_p, confnumcols_p
      end function CPXrefineconflict
      END INTERFACE

      INTERFACE
      function CPXrhssa(env,lpid,begin,finish,lower,upper)
!DEC$ ATTRIBUTES stdcall, alias:'CPXrhssa' :: CPXrhssa
!DEC$ ATTRIBUTES value :: env, lpid, begin, finish
!DEC$ ATTRIBUTES reference :: lower, upper
      use symtypes
      integer(IL) :: CPXrhssa
      integer(IL), intent(in) :: env, lpid, begin, finish
      real(RD), intent(out) :: lower(finish - begin + 1),               &
     &                         upper(finish - begin + 1)
      end function CPXrhssa
      END INTERFACE
!
      INTERFACE
      function CPXsetdblparam(env,whichparam,newvalue)
!DEC$ ATTRIBUTES stdcall, alias:'CPXsetdblparam' :: CPXsetdblparam
!DEC$ ATTRIBUTES value :: env, whichparam, newvalue
      use symtypes
      integer(IL) :: CPXsetdblparam
      integer(IL), intent(in) :: whichparam
      integer(ILL), intent(in) :: env
      real(RD), intent(in) :: newvalue
      end function CPXsetdblparam
      END INTERFACE
!
      INTERFACE
      function CPXsetdefaults(env)
!DEC$ ATTRIBUTES stdcall, alias:'CPXsetdefaults' :: CPXsetdefaults
!DEC$ ATTRIBUTES value :: env
      use symtypes
      integer(IL) :: CPXsetdefaults
      integer(IL), intent(in) :: env
      end function CPXsetdefaults
      END INTERFACE
!
      INTERFACE
      function CPXsetintparam(env,whichparam,onoff)
!DEC$ ATTRIBUTES stdcall, alias:'CPXsetintparam' :: CPXsetintparam
!DEC$ ATTRIBUTES value :: env, whichparam, onoff
      use symtypes
      integer(IL) :: CPXsetintparam
      integer(IL), intent(in) :: whichparam, onoff
      integer(ILL), intent(in) :: env
      end function CPXsetintparam
      END INTERFACE
!
      INTERFACE
      function CPXsetlogfilename(env,filename,action)
!DEC$ ATTRIBUTES stdcall, alias:'CPXsetlogfilename' :: CPXsetlogfilename
!DEC$ ATTRIBUTES value :: env
!DEC$ ATTRIBUTES reference :: filename, action
      use symtypes
      integer(IL) :: CPXsetlogfilename
      integer(ILL), intent(in) :: env
      character(*), intent(in) :: filename, action
      end function CPXsetlogfilename
      END INTERFACE
!
      INTERFACE
      function CPXsetlpcallbackfunc(env,funcpnt,privpnt)
!DEC$ ATTRIBUTES stdcall, alias:'CPXsetlpcallbackfunc' :: CPXsetlpcallbackfunc
!DEC$ ATTRIBUTES value :: env
!DEC$ ATTRIBUTES reference :: funcpnt, privpnt
      use symtypes
      integer(IL) :: CPXsetlpcallbackfunc
      integer(IL), intent(in) :: env, funcpnt, privpnt
      end function CPXsetlpcallbackfunc
      END INTERFACE
!
      INTERFACE
      function CPXsetmipcallbackfunc(env,funcpnt,privpnt)
!DEC$ ATTRIBUTES stdcall, alias:'CPXsetmipcallbackfunc' :: CPXsetmipcallbackfunc
!DEC$ ATTRIBUTES value :: env
!DEC$ ATTRIBUTES reference :: funcpnt, privpnt
      use symtypes
      integer(IL) :: CPXsetmipcallbackfunc
      integer(IL), intent(in) :: env, funcpnt, privpnt
      end function CPXsetmipcallbackfunc
      END INTERFACE
!
!!      INTERFACE
!!      function CPXsetstrparam(env,whichparam,newvalue)
!!!DEC$ ATTRIBUTES stdcall, alias:'CPXsetstrparam' :: CPXsetstrparam
!!!DEC$ ATTRIBUTES value :: env, whichparam, newvalue
!!      use symtypes
!!      integer(IL) :: CPXsetstrparam
!!      character(*), intent(in) :: newvalue
!!      integer(IL), intent(in) :: env, whichparam
!!      end function CPXsetstrparam
!!      END INTERFACE
!
      INTERFACE
      function CPXsolution(env,lpid,lpstat,objval,x,pi,slack,dj)
!DEC$ ATTRIBUTES stdcall, alias:'CPXsolution' :: CPXsolution
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: lpstat, objval, x, pi, slack, dj
      use symtypes
      integer(IL) :: CPXsolution
      integer(ILL), intent(in) :: env, lpid
      integer(IL), intent(out) :: lpstat
      real(RD), intent(out) :: objval
!!!      real(RD), intent(out) :: x(37300), pi(25900), slack(25900), dj(37300)
      real(RD), intent(out) :: x(*), pi(*), slack(*), dj(*)
      end function CPXsolution
      END INTERFACE
!
      INTERFACE
      function CPXsoswrite(env,lpid,filename)
!DEC$ ATTRIBUTES stdcall, alias:'CPXsoswrite' :: CPXsoswrite
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXsoswrite
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: env, lpid
      end function CPXsoswrite
      END INTERFACE
!
      INTERFACE
      function CPXstrcpy(tostr,fromstr)
!DEC$ ATTRIBUTES stdcall, alias:'CPXstrcpy' :: CPXstrcpy
!DEC$ ATTRIBUTES reference :: tostr, fromstr
      use symtypes
      integer(IL) :: CPXstrcpy
      character(*), intent(in) :: fromstr
      character(*), intent(out) :: tostr
      end function CPXstrcpy
      END INTERFACE
!
      INTERFACE
      function CPXstrlen(string)
!DEC$ ATTRIBUTES stdcall, alias:'CPXstrlen' :: CPXstrlen
!DEC$ ATTRIBUTES reference :: string
      use symtypes
      integer(IL) :: CPXstrlen
      character(*), intent(in) :: string
      end function CPXstrlen
      END INTERFACE
!
      INTERFACE
      function CPXtreewrite(env,lpid,filename)
!DEC$ ATTRIBUTES stdcall, alias:'CPXtreewrite' :: CPXtreewrite
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXtreewrite
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: env, lpid
      end function CPXtreewrite
      END INTERFACE
!
      INTERFACE
      function CPXvecwrite(env,lpid,filename)
!DEC$ ATTRIBUTES stdcall, alias:'CPXvecwrite' :: CPXvecwrite
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXvecwrite
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: env, lpid
      end function CPXvecwrite
      END INTERFACE
!
      INTERFACE
      function CPXversion(env)
!DEC$ ATTRIBUTES stdcall, alias:'CPXversion' :: CPXversion
!DEC$ ATTRIBUTES value :: env
      use symtypes
      integer(IL) :: CPXversion
      integer(IL), intent(in) :: env
      end function CPXversion
      END INTERFACE
!
      INTERFACE
      function CPXwriteprob(env,lpid,filename,filetype)
!DEC$ ATTRIBUTES stdcall, alias:'CPXwriteprob' :: CPXwriteprob
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename, filetype
      use symtypes
      integer(IL) :: CPXwriteprob
      character(*), intent(in) :: filename, filetype
      integer(ILL), intent(in) :: env, lpid
      end function CPXwriteprob
      END INTERFACE
!
      INTERFACE
      function CPXsolwrite(env,lpid,filename)
!DEC$ ATTRIBUTES stdcall, alias:'CPXsolwrite' :: CPXsolwrite
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXsolwrite
      character(*), intent(in) :: filename
      integer(ILL), intent(in) :: env, lpid
      end function CPXsolwrite
      END INTERFACE
!
      INTERFACE
      function  CPXwritemipstarts (env,lpid,filename,begin,endd)
!DEC$ ATTRIBUTES stdcall, alias:'CPXwritemipstarts' :: CPXwritemipstarts
!DEC$ ATTRIBUTES value :: env, lpid, begin, endd
!DEC$ ATTRIBUTES reference :: filename
      use symtypes
      integer(IL) :: CPXwritemipstarts
      character(*), intent(in) :: filename
      integer(IL), intent(in) :: begin, endd
      integer(ILL), intent(in) :: env, lpid
      end function CPXwritemipstarts
      END INTERFACE
!
      INTERFACE
      function CPXchgmipstart(env,lpid,cnt,indices,values)
!DEC$ ATTRIBUTES stdcall, alias:'CPXchgmipstart' :: CPXchgmipstart
!DEC$ ATTRIBUTES value :: env, lpid, cnt
!DEC$ ATTRIBUTES reference :: indices, values
      use symtypes
      integer(IL) :: CPXchgmipstart
      integer(IL), intent(in) :: env, lpid, cnt
      integer(IL), intent(in) :: indices(cnt)
      real(RD), intent(in) :: values(cnt)
      end function CPXchgmipstart
      END INTERFACE

      INTERFACE
      function CPXgetmiprelgap(env,lpid,gap)
!DEC$ ATTRIBUTES stdcall, alias:'CPXgetmiprelgap' :: CPXgetmiprelgap
!DEC$ ATTRIBUTES value :: env, lpid
!DEC$ ATTRIBUTES reference :: gap
      use symtypes
      integer(ILL) :: CPXgetmiprelgap
      integer(ILL), intent(in) :: env, lpid
      real(RD), intent(inout) :: gap
      end function CPXgetmiprelgap
      END INTERFACE


END MODULE cplex_ifaces

