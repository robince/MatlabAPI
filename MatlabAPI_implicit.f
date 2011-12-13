!*************************************************************************************
! 
!  MATLAB (R) is a trademark of The Mathworks (R) Corporation
! 
!  Function:    MatlabAPI_implicit
!  Filename:    MatlabAPI_implicit.f
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
!  2011/May/11 --> Added output for specific address information
!              --> Changed argument to loc function to be 1st element reference
! 
!*************************************************************************************

#include "fintrf.h"

!\
! The following macros are needed for older versions of MATLAB that do not have these
! macros defined in the fintrf.h file.
!/

#ifndef mwSize
#define mwSize integer(4)
#endif

#ifndef mwPointer
#define mwPointer integer(4)
#endif

#ifndef mwIndex
#define mwIndex integer(4)
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
      mwPointer :: locx, locfp
      mwSize :: stride, M, N
      character(len=200) :: line
      integer(4) :: k
!-COM
      common /xblock/ X
!-----
      k = mexPrint("--------------------------------------------------")
      k = mexPrint("MatlabAPI_implicit test function")
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
! Initialize the contiguous pointer.
!/
      k = mexPrint(" ")
      k = mexPrint("... Initializing X = 1,2,3,...")
      call initializeX
      k = mexPrint("... Getting contiguous pointer, fp => X")
      fp => X
      k = mexPrint("... Getting stride of target elements")
      stride = fpStride(fp)
      locx = loc(X(1,1))
      write(line,*) "... LOC(X)  = ",locx
      k = mexPrint(line(2:))
      locfp = loc(fp(1,1))
      write(line,*) "... LOC(fp) = ",locfp
      k = mexPrint(line(2:))
      write(line,*) "... Stride = ",stride
      k = mexPrint(line(2:))
      if( locx == locfp ) then
          k = mexPrint("... fp points to original X")
      else
          k = mexPrint("... fp points to copy of X")
      endif
      write(line,*) "... X(:,1)  = ",X(:,1)
      k = mexPrint(line(2:))
      write(line,*) "... fp(:,1) = ",fp(:,1)
      k = mexPrint(line(2:))
!\
! Now run the contiguous test, setting values directly
!/
      k = mexPrint(" ")
      k = mexPrint("... Setting fp slice = 4")
      fp = 4.d0
      write(line,*) "... X(:,1)  = ",X(:,1)
      k = mexPrint(line(2:))
      write(line,*) "... fp(:,1) = ",fp(:,1)
      k = mexPrint(line(2:))
      if( any( X /= 4.d0 ) ) then
          k = mexPrint("... Original X is not changed.")
          k = mexPrint("BAD UNEXPECTED RESULT. Contact author.")
      else
          k = mexPrint("... Original X is changed.")
      endif
      k = mexPrint(" ")
      k = mexPrint("... Setting X = 5")
      X = 5.d0
      write(line,*) "... X(:,1)  = ",X(:,1)
      k = mexPrint(line(2:))
      write(line,*) "... fp(:,1) = ",fp(:,1)
      k = mexPrint(line(2:))
      if( any( fp /= 5.d0 ) ) then
          k = mexPrint("... Pointer fp target is not changed.")
          k = mexPrint("BAD UNEXPECTED RESULT. Contact author.")
      else
          k = mexPrint("... Pointer fp target is changed.")
      endif
!\
! Now run the contiguous test, pass the pointer implicitly
!/
      k = mexPrint(" ")
      k = mexPrint("... Initializing X = 1,2,3,...")
      call initializeX
      k = mexPrint("... call implicit(X,4,6,LOC(X))")
      M = 4
      N = 6
      call implicit(X,M,N,locx)
      if( any( fp /= 1.d0 ) ) then
          k = mexPrint("... Pointer fp target is not changed.")
          k = mexPrint("BAD UNEXPECTED RESULT. Contact author.")
      else
          k = mexPrint("... Pointer fp target is changed.")
      endif
      k = mexPrint(" ")
      k = mexPrint("... Initializing X = 1,2,3,...")
      call initializeX
      k = mexPrint("... call implicit(fp,4,6,LOC(fp))")
      M = 4
      N = 6
      call implicit(fp,M,N,locfp)
      if( any( X /= 1.d0 ) ) then
          k = mexPrint("... Original X is not changed.")
          k = mexPrint("BAD UNEXPECTED RESULT. Contact author.")
      else
          k = mexPrint("... Original X is changed as expected.")
      endif
      k = mexPrint(" ")
      k = mexPrint("... Initializing X = 1,2,3,...")
      call initializeX
      k = mexPrint("... call implicit(VAL(LOC(fp)),4,6,LOC(fp))")
      M = 4
      N = 6
      call implicit(%VAL(loc(fp(1,1))),M,N,locfp)
      if( any( X /= 1.d0 ) ) then
          k = mexPrint("... Original X is not changed.")
          k = mexPrint("BAD UNEXPECTED RESULT. Contact author.")
      else
          k = mexPrint("... Original X is changed as expected.")
      endif
!\
! Initialize the non-contiguous pointer.
!/
      k = mexPrint(" ")
      k = mexPrint("... Initializing X = 1,2,3,...")
      call initializeX
      k = mexPrint(" ")
      k = mexPrint("... Getting non-contiguous pointer fp => X(::2,:)")
      fp => X(::2,:)
      locx = loc(X(1,1))
      write(line,*) "... LOC(X)  = ",locx
      k = mexPrint(line(2:))
      locfp = loc(fp(1,1))
      write(line,*) "... LOC(fp) = ",locfp
      k = mexPrint(line(2:))
      stride = fpStride(fp)
      write(line,*) "... Stride  = ",stride
      k = mexPrint(line(2:))
      if( locx == locfp ) then
          k = mexPrint("... fp points to original X")
      else
          k = mexPrint("... fp points to copy of X")
      endif
      write(line,*) "... X shape  = ",size(X,1)," x ",size(X,2)
      k = mexPrint(line(2:))
      write(line,*) "... fp shape = ",size(fp,1)," x ",size(fp,2)
      k = mexPrint(line(2:))
      write(line,*) "... X(:,1)  = ",X(:,1)
      k = mexPrint(line(2:))
      write(line,*) "... fp(:,1) = ",fp(:,1)
      k = mexPrint(line(2:))
!\
! Now run the non-contiguous test, setting values directly
!/
      k = mexPrint(" ")
      k = mexPrint("... Setting fp slice = 3")
      fp = 3.d0
      if( any( X(::2,:) /= 3.d0 ) ) then
          k = mexPrint("... Original X is not changed.")
          k = mexPrint("BAD UNEXPECTED RESULT. Contact author.")
      else
          k = mexPrint("... Original X is changed.")
      endif
      k = mexPrint(" ")
      k = mexPrint("... Setting X = 5")
      X = 5.d0
      write(line,*) "... X(:,1)  = ",X(:,1)
      k = mexPrint(line(2:))
      write(line,*) "... fp(:,1) = ",fp(:,1)
      k = mexPrint(line(2:))
      if( any( fp /= 5.d0 ) ) then
          k = mexPrint("... Pointer fp target is not changed.")
          k = mexPrint("BAD UNEXPECTED RESULT. Contact author.")
      else
          k = mexPrint("... Pointer fp target is changed.")
      endif
!\
! Now run the non-contiguous test, pass the pointer implicitly
!/
      k = mexPrint(" ")
      k = mexPrint("... Initializing X = 1,2,3,...")
      call initializeX
      k = mexPrint("... call implicit2(fp,2,6,LOC(fp))")
      M = 2
      N = 6
      call implicit2(fp,M,N,locfp)
      if( any( X(::2,:) /= 2.d0 ) ) then
          k = mexPrint("... Original X is not changed.")
          k = mexPrint("BAD UNEXPECTED RESULT. Contact author.")
      else
          k = mexPrint("... Original X is changed.")
      endif
!\
! Done
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint(" ")
      k = mexPrint("... Done")
      k = mexPrint(" ")
      return

      contains

!----------------------------------------------------------------------------

      subroutine initializeX
      X(:,1) = (/  1.d0,  2.d0,  3.d0,  4.d0 /)
      X(:,2) = (/  5.d0,  6.d0,  7.d0,  8.d0 /)
      X(:,3) = (/  9.d0,  9.d0, 10.d0, 11.d0 /)
      X(:,4) = (/ 13.d0, 14.d0, 15.d0, 16.d0 /)
      X(:,5) = (/ 17.d0, 18.d0, 19.d0, 20.d0 /)
      X(:,6) = (/ 21.d0, 22.d0, 23.d0, 24.d0 /)
      return
      end subroutine initializeX

      end subroutine mexFunction

!----------------------------------------------------------------------------
!\
! The following routines, since they are not contained in another programming
! unit, have an implicit interface. As such, they cannot use assumed shape
! arguments and one is forced to pass the sizes manually.
!/
!----------------------------------------------------------------------------

      subroutine Implicit(Y,M,N,address)
      use MatlabAPImex
      implicit none
!-ARG
      mwSize, intent(in) :: M, N
      real(8), intent(inout) :: Y(M,N)  ! Passed size
      mwPointer, intent(in) :: address
!-LOC
      mwPointer :: locy
      integer(4) :: k
      character(len=200) :: line
!-COM
      real(8) :: X(4,6)
      common /xblock/ X
!-----
      k = mexPrint("Inside subroutine Implicit, contiguous case")
      write(line,*) "... Y shape  = ",size(Y,1)," x ",size(Y,2)
      k = mexPrint(line(2:))
      locy = loc(Y(1,1))
      write(line,*) "... LOC(Y)  = ",locy
      k = mexPrint(line(2:))
      write(line,*) "... LOC(fp) = ",address
      k = mexPrint(line(2:))
      write(line,*) "... Y(:,1)  = ",Y(:,1)
      k = mexPrint(line(2:))
      if( locy == address ) then
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
          k = mexPrint("    call routine(VAL(LOC(fp(1,1))),...)")
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
      k = mexPrint("... Setting Y = 1")
      Y = 1.d0
      write(line,*) "... Y(:,1)  = ",Y(:,1)
      k = mexPrint(line(2:))
      k = mexPrint("... Checking to see if original X changed.")
      write(line,*) "... X(:,1)  = ",X(:,1)
      k = mexPrint(line(2:))
      if( any( X /= Y ) ) then
          k = mexPrint("... Original X is not changed yet.")
      else
          k = mexPrint("... Original X is changed.")
      endif
      return
      end subroutine Implicit

!----------------------------------------------------------------------

      subroutine Implicit2(Y,M,N,address)
      use MatlabAPImex
      implicit none
!-ARG
      mwSize, intent(in) :: M, N
      real(8), intent(inout) :: Y(M,N)  ! Passed size
      mwPointer, intent(in) :: address
!-LOC
      mwPointer :: locy
      integer(4) :: k
      character(len=200) :: line
!-COM
      real(8) :: X(4,6)
      common /xblock/ X
!-----
      k = mexPrint("Inside subroutine Implicit2, non-contiguous case")
      write(line,*) "... Y shape  = ",size(Y,1)," x ",size(Y,2)
      k = mexPrint(line(2:))
      locy = loc(Y(1,1))
      write(line,*) "... LOC(Y)  = ",locy
      k = mexPrint(line(2:))
      write(line,*) "... address = ",address
      k = mexPrint(line(2:))
      write(line,*) "... Y(:,1)  = ",Y(:,1)
      k = mexPrint(line(2:))
      if( locy == address ) then
          k = mexPrint("BAD UNEXPECTED RESULT. Contact author.")
      else
          k = mexPrint("Expected Result, Address of Copy was passed")
      endif
      k = mexPrint("... Setting Y = 2")
      Y = 2.d0
      write(line,*) "... Y(:,1)  = ",Y(:,1)
      k = mexPrint(line(2:))
      k = mexPrint("... Checking to see if original X changed.")
      write(line,*) "... X(:,1)  = ",X(:,1)
      k = mexPrint(line(2:))
      if( any( X(::2,:) /= Y ) ) then
          k = mexPrint("... Original X is not changed yet.")
      else
          k = mexPrint("... Original X is changed.")
      endif
      return
      end subroutine Implicit2
