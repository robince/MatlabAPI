!*************************************************************************************
! 
!  MATLAB (R) is a trademark of The Mathworks (R) Corporation
! 
!  Function:    MatlabAPI_getset
!  Filename:    MatlabAPI_getset.f
!  Programmer:  James Tursa
!  Version:     1.00
!  Date:        October 27, 2009
!  Copyright:   (c) 2009 by James Tursa, All Rights Reserved
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
!  This routine generates a plot of a spiral. On repeated calls, the background
!  color of the axis will change to random colors. e.g., at the MATLAB command
!  line do this:
!
!  >> MatlabAPI_getset
!  >> MatlabAPI_getset
!  >> MatlabAPI_getset
!  >> MatlabAPI_getset
!           :
!          etc.
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
      mwPointer lhs(1), rhs(3)            ! Really mxArray *[]
      mwPointer color_old, color_new      ! Really mxArray *
      real(8), pointer :: fpx(:)          ! Used for pointing to data
      real(8), pointer :: fpy(:)          !  areas of mxArray variables
      character(len=80) title
      integer*4 k
      integer :: init = 1
      real(8) :: handle
      mwSize :: n1,n2
!-SAV
      save init, handle
!-----
!\
! Check for no inputs or outputs.
!/
      if( nrhs /= 0 .or. nlhs /= 0 ) then
          call mexErrMsgTxt("This function has no inputs or outputs")
      endif
!\
! Put up an arbitrary plot, save the axis handle. To make the sprial
! data, first get 1D pointers into the x and y data areas of the
! mxArray variables we will be using to plot. Then pass then along to
! a subroutine to fill in the data. Note that we are using an explicit
! interface for the makespiral call, so the shape of the arguments will
! get passed along since the subroutine has the inputs declared as
! assumed shape. Save the handle to the axis in a variable that has
! the save attribute so it will retain its value between calls.
!/
      if( init == 1 ) then
           n1 = 1
           n2 = 200
           rhs(1) = mxCreateDoubleMatrix(n1,n2,mxREAL)
           fpx => fpGetPr1(rhs(1))
           rhs(2) = mxCreateDoubleMatrix(n1,n2,mxREAL)
           fpy => fpGetPr1(rhs(2))
           call makespiral(fpx,fpy)
           rhs(3) = mxCreateString("-o")
           k = mexCallMATLAB(0,lhs,0,rhs,"figure")
           k = mexCallMATLAB(0,lhs,3,rhs,"plot")
           call mxDestroyArray(rhs(3))
           call mxDestroyArray(rhs(2))
           call mxDestroyArray(rhs(1))
           k = mexCallMATLAB(1,lhs,0,rhs,"gca")
           handle = mxGetScalar(lhs(1))
           call mxDestroyArray(lhs(1))
           init = 0
      endif
!\
! Get the "Color" property associated with the handle using the mexGet
! routine. Note that the mexGet routine does not come with the MATLAB
! API ... it is a routine written with custom code in the MatlabAPImex
! module. Get a 1D pointer into the data area.
!/
      color_old = mexGet(handle,"Color")
      if( color_old == 0 ) then
          init = 1
          call mexErrMsgTxt("Could not get the 'Color' property")
      endif
      fpx => fpGetPr1(color_old)
!\
! Make a copy of the "Color" property and get a 1D pointer into the
! data area.
!/
      color_new = mxDuplicateArray(color_old)
      fpy => fpGetPr1(color_new)
!\
! Change the colors to random colors using the Fortran intrinsic
! subroutine random_number. Note that this routine takes the shape
! of the input automatically and fills all the values.
!/
      call random_number(fpy)
!\
! Reset the "Color" property to use the new colors. Note that the mexSet
! routine, like mexGet, does not come with the MATLAB API ... it is a
! routine written with custom code in the MatlabAPImex module.
!/
      if( mexSet(handle, "Color", color_new) /= 0 ) then
          call mexErrMsgTxt("Could not set a new 'Color' property")
      endif
!\
! Change title to reflect the old & new colors
!/
      write(title,1) fpx, fpy
    1 format('Old colors = (',2(F6.3,','),F6.3,' ), ',                  &
     &       'New colors = (',2(F6.3,','),F6.3,' )')
      rhs(1) = mxCreateDoubleScalar(handle)
      rhs(2) = mxCreateString(title)
      k = mexCallMATLAB(0,lhs,2,rhs,"title")
!\
! Clean-up
!/
      call mxDestroyArray(rhs(2))
      call mxDestroyArray(rhs(1))
      call mxDestroyArray(color_new)
      call mxDestroyArray(color_old)
      return

      contains

!---------------------------------------------------------------------

      subroutine makespiral(fpx,fpy)
      implicit none
!-ARG
      real(8), intent(out) :: fpx(:), fpy(:)
!-LOC
      real(8) a, r
      integer i
!-----
      r = 0.d0
      do i=1,size(fpx)
          r = r + 0.01d0
          a = (i * 10.d0 * 3.14d0 / 180.d0)
          fpx(i) = r * cos(a)
          fpy(i) = r * sin(a)
      enddo
      return
      end subroutine makespiral

!---------------------------------------------------------------------

      end subroutine mexFunction
