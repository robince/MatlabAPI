!*************************************************************************************
! 
!  MATLAB (R) is a trademark of The Mathworks (R) Corporation
! 
!  Function:    MatlabAPI_copy
!  Filename:    MatlabAPI_copy.f
!  Programmer:  James Tursa
!  Version:     1.10
!  Date:        June 20, 2011
!  Copyright:   (c) 2009, 2011 by James Tursa, All Rights Reserved
! 
!   This code uses the BSD License:
! 
!   Redistribution and use in source and binary forms, with or without 
!   modification, are permitted provided that the following conditions are 
!   met:
! 
!      * Redistributions of source code must retain the above copyright 
!        notice, this list of conditions and the following disclaimer.
!      * Redistributions in binary form must reproduce the above copyright 
!        notice, this list of conditions and the following disclaimer in 
!        the documentation and/or other materials provided with the distribution
!       
!   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
!   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
!   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
!   ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
!   LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
!   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
!   SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
!   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
!   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
!   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
!   POSSIBILITY OF SUCH DAMAGE.
!
!  This function takes not inputs and generates no outputs. What it does is go through
!  a series of MATLAB API function calls to demonstrate the use of the module functions
!  in the following files:
!
!  MatlabAPImx.f   - Interface definitions for the MATLAB API mx___ functions
!  MatlabAPImex.f  - Interface definitions for the MATLAB API mex___ functions
!
!  Both files have extra routines that are not part of the original MATLAB API
!  function list. Some of the extra routines are drop-in replacements for original
!  MATLAB API functions that were not part of the Fortran API. Other routines are
!  brand new ... e.g. all of the Fortran pointer routines. See comments in these
!  files for details on the interface for these functions.
!
!  This routine goes through a series of data copy functions to demonstrate the
!  usage of the mxCopy___ routines, and how the equivalent functionality can be
!  obtained with the use of some Fortran pointer routines. To run the examples:
!
!  >> MatlabAPI_copy
! 
!*************************************************************************************

#include "fintrf.h"

!\
! The following macros are needed for older versions of MATLAB that do not have these
! macros defined in the fintrf.h file.
!/

#ifndef mwSize
#define mwSize integer*4
#endif

#ifndef mwPointer
#define mwPointer integer*4
#endif

#ifndef mwIndex
#define mwIndex integer*4
#endif

!---------------------------------------------------------------------

      subroutine mexFunction(nlhs, plhs, nrhs, prhs)
      use MatlabAPImex
      use MatlabAPImx
      implicit none
!-ARG
      mwPointer plhs(*), prhs(*)
      integer*4 nlhs, nrhs
!-LOC
      mwPointer mx
      mwPointer pr
      real(8), pointer :: fp(:)
      real(8) r8(3)
      integer(4) i4(3)
      character(len=200) line
      integer*4 k, classid
      mwSize :: m = 1, n = 3
!-----
!\
! Check for no inputs or outputs.
!/
      if( nrhs /= 0 .or. nlhs /= 0 ) then
          call mexErrMsgTxt("This function has no inputs or outputs")
      endif
!\
! Do some mxCopy___ tests with real*8 data type
!/
      mx = mxCreateDoubleMatrix(1, 3, mxREAL)  ! Create the mxArray
      pr = mxGetPr(mx)                         ! Get pointer to mxArray data
      fp => fpGetPr1(mx)                       ! Get Fortran pointer to mxArray data

      k = mexPrint(" ")
      k = mexPrint("Copying vector [1 2 3] from Real8 to mxArray")
      k = mexPrint("using the mxCopyReal8ToPtr routine")
      k = mexPrint(" ")
      r8 = (/ 1.d0, 2.d0, 3.d0 /)              ! Fill the real*8 vector
      call mxCopyReal8ToPtr(r8, pr, n)         ! Copy real*8 to mxArray data
      write(line,*) "r8 = ",r8
      k = mexPrint(line)
      write(line,*) "mx = ",fp
      k = mexPrint(line)
      
      k = mexPrint(" ")
      k = mexPrint("Copying vector [4 5 6] from mxArray to Real8")
      k = mexPrint("using the mxCopyPtrToReal8 routine")
      k = mexPrint(" ")
      fp = (/ 4.d0, 5.d0, 6.d0 /)              ! Fill the mxArray data with vector
      call mxCopyPtrToReal8(pr, r8, n)         ! Copy the mxArray data to real*8
      write(line,*) "mx = ",fp
      k = mexPrint(line)
      write(line,*) "r8 = ",r8
      k = mexPrint(line)

      k = mexPrint(" ")
      k = mexPrint("Copying vector [1 2 3] from Real8 to mxArray")
      k = mexPrint("using the fpGetPr1 routine with direct assignment")
      k = mexPrint(" ")
      r8 = (/ 1.d0, 2.d0, 3.d0 /)              ! Fill the real*8 vector
      fp = r8                                  ! Copy real*8 to mxArray data
      write(line,*) "r8 = ",r8
      k = mexPrint(line)
      write(line,*) "mx = ",fp
      k = mexPrint(line)
      
      k = mexPrint(" ")
      k = mexPrint("Copying vector [4 5 6] from mxArray to Real8")
      k = mexPrint("using the fpGetPr1 routine with direct assignment")
      k = mexPrint(" ")
      fp = (/ 4.d0, 5.d0, 6.d0 /)              ! Fill the mxArray data with vector
      r8 = fp                                  ! Copy the mxArray data to real*8
      write(line,*) "mx = ",fp
      k = mexPrint(line)
      write(line,*) "r8 = ",r8
      k = mexPrint(line)
      
      call mxDestroyArray(mx)
!\
! Do some mxCopy___ tests with integer*4 data type
!/
      classid = mxClassIDFromClassName('int32')
      mx = mxCreateNumericMatrix(m, n, classid, mxREAL)
      pr = mxGetData(mx)

      k = mexPrint(" ")
      k = mexPrint("Copying vector [1 2 3] from Integer4 to mxArray")
      k = mexPrint(" ")
      i4 = (/ 1, 2, 3 /)
      call mxCopyInteger4ToPtr(i4, pr, n)
      write(line,*) "i4 = ",i4
      k = mexPrint(line)
      call display3i4(%val(pr))
      
      k = mexPrint(" ")
      k = mexPrint("Copying vector [4 5 6] from mxArray to Integer4")
      k = mexPrint(" ")
      call fill3i4(%val(pr))
      call mxCopyPtrToInteger4(pr, i4, n)
      call display3i4(%val(pr))
      write(line,*) "i4 = ",i4
      k = mexPrint(line)

      call mxDestroyArray(mx)
      
      end subroutine mexFunction
!----------------------------------------------------------------------
      subroutine fill3i4(i4)
      implicit none
!-ARG
      integer(4) i4(3)
!-----
      i4 = (/ 4, 5, 6 /)
      return
      end subroutine fill3i4
!----------------------------------------------------------------------
      subroutine display3i4(i4)
      use MatlabAPImex
      implicit none
!-ARG
      integer(4) i4(3)
!-LOC
      character(len=200) line
      integer*4 k
!-----
      write(line,*) "mx = ",i4
      k = mexPrint(line)
      return
      end subroutine display3i4
