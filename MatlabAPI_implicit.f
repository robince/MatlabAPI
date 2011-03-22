!*************************************************************************************
! 
!  MATLAB (R) is a trademark of The Mathworks (R) Corporation
! 
!  Function:    MatlabAPI_implicit
!  Filename:    MatlabAPI_implicit.f
!  Programmer:  James Tursa
!  Version:     1.01
!  Date:        December 11, 2009
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
!  This function takes no inputs and generates no outputs. What it does is check for
!  a design flaw in the Fortran compiler. When a Fortran pointer is passed to a
!  routine through an implicit interface, a good compiler will check to see whether
!  the target elements are contiguous in memory. If so, then the address of the
!  target is passed to the called routine. If not, then a copy-in copy-out scheme
!  is used instead. E.g., the Intel Fortran compilers have this desired behavior.
!  Some compilers, however (e.g. the Compaq Fortran compilers), will *always* do
!  the copy-in copy-out behavior. This is undesireable if you are working with very
!  large arrays as the temporary copy typically comes from the stack and you can
!  easily overflow the stack and bomb your program.
!
!  The output from this routine is simply text output that demonstrates the progress
!  of the routine as it is running.
!
!  Change Log:
!  2009/Oct/27 --> Initial Release
!  2009/Dec/11 --> Changed default address function to LOC instead of %LOC
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

!\
! This is in case a compiler supports %LOC but not LOC
!/

#ifdef PERCENTLOC
#define loc %LOC
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
      real(8), target :: X(4,6)
      real(8), pointer :: fp(:,:)
      mwPointer :: locx
      mwSize :: stride
      character(len=80) :: line
      integer(4) :: k
!-----
!\
! Check the input
!/
      if( nrhs /= 0 ) then
          call mexErrMsgTxt("This function has no inputs")
      endif
      if( nlhs /= 0 ) then
          call mexErrMsgTxt("This function returns no outputs")
      endif
!\
! Initialize the pointer and then pass it implicitly.
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint("MatlabAPI_implicit test function")
      k = mexPrint(" ")
      k = mexPrint("... Getting Fortran pointer, fp => X")
      fp => X
      k = mexPrint("... Getting stride of target elements")
      stride = fpStride(fp)
      write(line,*) "... Stride = ",stride
      k = mexPrint(line(2:))
!\
! Now run the tests
!/
      k = mexPrint(" ")
      k = mexPrint("... call implicit(fp,4,6,LOC(fp))")
      locx = loc(fp)
      call implicit(fp,4,6,locx)
      k = mexPrint(" ")
      k = mexPrint("... call implicit(VAL(LOC(fp)),4,6,LOC(fp))")
      locx = loc(fp)
      call implicit(%VAL(loc(fp)),4,6,locx)
      k = mexPrint(" ")
      k = mexPrint("... Getting non-contiguous Fortran pointer fp")
      fp => X(::2,:)
      locx = loc(fp)
      stride = fpStride(fp)
      write(line,*) "... Stride = ",stride
      k = mexPrint(line(2:))
      k = mexPrint("... call implicit2(fp,2,6,LOC(fp))")
      call implicit2(fp,2,6,locx)
!\
! Done
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint(" ")
      k = mexPrint("... Done")
      k = mexPrint(" ")
      return
      end subroutine mexFunction
!----------------------------------------------------------------------
!\
! The following routine, since it is not contained in another programming
! unit, has an implicit interface. As such, it cannot use assumed shape
! arguments and one is forced to pass the sizes manually.
!/
!----------------------------------------------------------------------
      subroutine Implicit(X,M,N,address)
      use MatlabAPImex
      implicit none
!-ARG
      mwSize, intent(in) :: M, N
      real(8), intent(in) :: X(M,N)  ! Passed shape A
      mwPointer, intent(in) :: address
!-LOC
      mwPointer :: locx
      integer(4) :: k
!-----
      locx = loc(X)
      if( locx == address ) then
          k = mexPrint("Good, Address of Target was passed")
      else
          k = mexPrint("Bad, Address of Copy was passed.")
          k = mexPrint("Your compiler, although conforming, has a")
          k = mexPrint("design flaw for passing contiguous targets.")
          k = mexPrint(" ")
          k = mexPrint("To get around this limitation, particularly")
          k = mexPrint("when working with vary large arrays, you")
          k = mexPrint("can use the following construct when calling")
          k = mexPrint("routines with implicit interfaces:")
          k = mexPrint("use MatlabAPImx")
          k = mexPrint("if( fpStride(fp) == 1 ) then")
          k = mexPrint("    call routine(VAL(LOC(fp)),...)")
          k = mexPrint("else")
          k = mexPrint("    call routine(fp,...)")
          k = mexPrint("endif")
          k = mexPrint(" ")
          k = mexPrint("Basically, the fpStride function returns a")
          k = mexPrint("1 if all the elements of the target are")
          k = mexPrint("contiguous in memory. If the stride is other")
          k = mexPrint("than 1 then fpStride will return that number")
          k = mexPrint("instead. Or if there is no consistent stride")
          k = mexPrint("in the target then fpStride will return 0.")
          k = mexPrint(" ")
      endif
      return
      end subroutine Implicit
!----------------------------------------------------------------------
      subroutine Implicit2(X,M,N,address)
      use MatlabAPImex
      implicit none
!-ARG
      mwSize, intent(in) :: M, N
      real(8), intent(in) :: X(M,N)  ! Passed shape A
      mwPointer, intent(in) :: address
!-LOC
      mwPointer :: locx
      integer(4) :: k
!-----
      locx = loc(X)
      if( locx == address ) then
          k = mexPrint("BAD UNEXPECTED RESULT. Contact author.")
      else
          k = mexPrint("Expected Result, Address of Copy was passed")
      endif
      return
      end subroutine Implicit2
