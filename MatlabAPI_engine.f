!*************************************************************************************
! 
!  MATLAB (R) is a trademark of The Mathworks (R) Corporation
! 
!  Filename:    MatlabAPI_engine.f
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
!  This program goes through a series of MATLAB API function calls to demonstrate
!  the use of the module functions in the following files:
!
!  MatlabAPImx.f   - Interface definitions for the MATLAB API mx___ functions
!  MatlabAPIeng.f  - Interface definitions for the MATLAB API eng___ functions
!
!  Both files have extra routines that are not part of the original MATLAB API
!  function list. Some of the extra routines are drop-in replacements for original
!  MATLAB API functions that were not part of the Fortran API. Other routines are
!  brand new ... e.g. all of the Fortran pointer routines. See comments in these
!  files for details on the interface for these functions.
!
!  This routine generates a plot of a spiral. On repeated calls, the background
!  color of the axis will change to random colors. Enter any non-blank input to
!  quit.
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

      Program GetSet
      use MatlabAPIeng
      use MatlabAPImx
      implicit none
!-LOC
      mwPointer ep                           ! Really Engine *
      mwPointer lhs(1), rhs(3)               ! Really mxArray *[]
      mwPointer color_old, color_new, mx     ! Really mxArray *
      real(8), pointer :: fpx(:)             ! Used for pointing to data
      real(8), pointer :: fpy(:)             !  areas of mxArray variables
      character(len=NameLengthMaxEng), pointer :: names(:) ! Engine workspace names
      character(len=80) title
      character c
      integer*4 k
      integer :: init = 1
      real(8) :: handle
      mwSize :: n1,n2
!-SAV
      save init, handle
!-----
!\
! Open the engine
!/
      ep = engOpen('')
      if( ep == 0 ) then
          write(*,*) "Engine did not open. One possible reason is"
          write(*,*) "MATLAB is not a registered server."
          write(*,*) "From Windows, open a Command Prompt and enter:"
          write(*,*) ">matlab /regserver"
          stop
      endif
!\
! First demonstrate putting variables into the engine workspace and
! then retrieving a list of all the names.
!/
      write(*,*) "Creating variables in the Engine workspace."
      mx = mxCreateDoubleScalar(1.d0)
      k = engPutVariable(ep, "First_Name", mx)
      k = engPutVariable(ep, "Second_Name", mx)
      k = engPutVariable(ep, "Third_Name", mx)
      k = engPutVariable(ep, "Fourth_Name", mx)
      call mxDestroyArray(mx)
      write(*,*) "Getting Engine workspace variable name list:"
      names => fpEngGetNames(ep)
      if( associated(names) ) then
          do k=1,size(names)
              write(*,*) names(k)
          enddo
          write(*,*) "Deallocating the name list."
          call fpDeallocate1CharacterEng(names)
      else
          write(*,*) 'No Names Found! Internal Error, contact author.'
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
  100 continue
      if( init == 1 ) then
           write(*,*) "Creating initial plot"
           n1 = 1
           n2 = 200
           rhs(1) = mxCreateDoubleMatrix(n1,n2,mxREAL)
           fpx => fpGetPr1(rhs(1))
           rhs(2) = mxCreateDoubleMatrix(n1,n2,mxREAL)
           fpy => fpGetPr1(rhs(2))
           call makespiral(fpx,fpy)
           rhs(3) = mxCreateString("-o")
           k = engCallMATLAB(ep,0,lhs,0,rhs,"figure")
           k = engCallMATLAB(ep,0,lhs,3,rhs,"plot")
           call mxDestroyArray(rhs(3))
           call mxDestroyArray(rhs(2))
           call mxDestroyArray(rhs(1))
           k = engCallMATLAB(ep,1,lhs,0,rhs,"gca")
           handle = mxGetScalar(lhs(1))
           call mxDestroyArray(lhs(1))
           init = 0
      endif
!\
! Get the "Color" property associated with the handle using the engGet
! routine. Note that the engGet routine does not come with the MATLAB
! API ... it is a routine written with custom code in the MatlabAPIeng
! module. Get a 1D pointer into the data area.
!/
      write(*,*) 'Getting current color'
      color_old = engGet(ep,handle,"Color")
      if( color_old == 0 ) then
          init = 1
          write(*,*) "Could not get the 'Color' property"
          write(*,*) "Generating new plot"
          goto 100
      endif
      fpx => fpGetPr1(color_old)
!\
! Make a copy of the "Color" property and get a 1D pointer into the
! data area.
!/
      write(*,*) 'Making new color'
      color_new = mxDuplicateArray(color_old)
      fpy => fpGetPr1(color_new)
!\
! Change the colors to random colors using the Fortran intrinsic
! subroutine random_number. Note that this routine takes the shape
! of the input automatically and fills all the values.
!/
      call random_number(fpy)
!\
! Reset the "Color" property to use the new colors. Note that the engSet
! routine, like engGet, does not come with the MATLAB API ... it is a
! routine written with custom code in the MatlabAPIeng module.
!/
      write(*,*) 'Setting new color'
      if( engSet(ep, handle, "Color", color_new) /= 0 ) then
          write(*,*) "Could not set a new 'Color' property"
      endif
!\
! Change title to reflect the old & new colors
!/
      write(title,1) fpx, fpy
    1 format('Old colors = (',2(F6.3,','),F6.3,' ), ',                  &
     &       'New colors = (',2(F6.3,','),F6.3,' )')
      write(*,*) title
      rhs(1) = mxCreateDoubleScalar(handle)
      rhs(2) = mxCreateString(title)
      k = engCallMATLAB(ep,0,lhs,2,rhs,"title")
!\
! Clean-up
!/
      call mxDestroyArray(rhs(2))
      call mxDestroyArray(rhs(1))
      call mxDestroyArray(color_new)
      call mxDestroyArray(color_old)
!\
! Run again?
!/
      write(*,*) "Press Enter to run again, or any other key to quit"
      c = ' '
      read(*,'(a)') c
      if( c == ' ' ) goto 100
      k = engClose(ep)
      stop

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

      end program GetSet
