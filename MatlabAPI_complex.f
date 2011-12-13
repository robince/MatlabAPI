!*************************************************************************************
! 
!  MATLAB (R) is a trademark of The Mathworks (R) Corporation
! 
!  Function:    MatlabAPI_complex
!  Filename:    MatlabAPI_complex.f
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
!  This function takes not inputs and generates one output. What it does is go through
!  a series of MATLAB API function calls to demonstrate the use of the module functions
!  for complex matrices in the following files:
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
!  The output from this routine is simply text output that demonstrates the progress
!  of the routine as it is running.
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
      mwPointer :: mx
      complex(8), allocatable :: B(:,:)
      real(8), pointer :: Afp2D(:,:), Afi2D(:,:)
      complex(8), pointer :: Cfz3D(:,:,:)
      complex(8), pointer :: Bfz2D(:,:), Cfz2D(:,:)
      complex(8), pointer :: Cfz1D(:)
      real(8), pointer :: Zfp1D(:), Zfi1D(:)
      character(len=63), pointer :: names(:)
      mwPointer :: Apr
      mwSize :: M, N
      integer :: istat
      integer*4 :: i, j, k
      mwSize :: n1,n2,n3
!-----
!\
! Check the input
!/
      if( nrhs /= 0 ) then
          call mexErrMsgTxt("This function has no inputs")
      endif
      if( nlhs > 1 ) then
          call mexErrMsgTxt("This function returns only one output")
      endif
!\
! Generate a 2D double matrix for testing. Get a Fortran pointer to the
! real & imag data and fill it with random numbers. Note that we are
! getting direct access to the data area of the mxArray variable mx
! through the use of the fpGetPr function. No data copying is involved.
! Also, the Fortran pointers can be used as regular Fortran arrays
! for all calculations in this routine. No need to use the messy %VAL()
! construct (that's buried behind the fpGetPr function). In this example,
! all of the pointers returned by the fpGetPr and fpGetPi functions point
! directly at the mxArrar data areas. No copies are involved. But for the
! fpGetPzCopy call, the returned pointer points to a copy of the data.
! The shape of the data area is automatically calculated inside the
! fpGetPr, fpGetPi, and fpGetPz functions and is a part of the Fortran
! pointer that is returned. Neat! For C programmers, note the difference
! here between Fortran pointers and C pointers. C pointers contain *only*
! a raw address and the type of object being pointed to (typically).
! Fortran pointers, however, always carry along with them not only the
! type being pointed to, but the shape and stride of the target. If you
! call a routine that expects a matrix as input, like the subroutines
! DisplayMatrixExplicit2z and DisplayMatrixExplicit3z, then the address
! of the target will automatically be passed. I.e., it is like the
! pointer gets automatically dereferenced everywhere it is used as an
! array instead of a pointer. Nice!
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint("MatlabAPI_complex test function")
      k = mexPrint(" ")
      k = mexPrint("... Generating a 3 x 4 complex matrix for testing")
      n1 = 3
      n2 = 4
      mx = mxCreateDoubleMatrix(n1, n2, mxCOMPLEX)
      k = mexPrint("... Getting 1D Fortran pointer to real data Zfp1D")
      Zfp1D => fpGetPr1(mx)
      if( .not.associated(Zfp1D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to associate pointer Zfp1D")
      endif
      k = mexPrint("... Getting 1D Fortran pointer to imag data Zfi1D")
      Zfi1D => fpGetPi1(mx)
      if( .not.associated(Zfi1D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to associate pointer Zfi1D")
      endif
      k = mexPrint("... Filling real data Zfp1D with natural numbers")
      Zfp1D = (/ (i, i=1,size(Zfp1D)) /)
      k = mexPrint("... Filling imag data Zfi1D with -natural numbers")
      Zfi1D = (/ (-i, i=1,size(Zfi1D)) /)
      k = mexPrint("... Getting 2D Fortran pointer to real data Afp2D")
      Afp2D => fpGetPr(mx)
      if( .not.associated(Afp2D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to associate pointer Afp2D")
      endif
      k = mexPrint("... Getting 2D Fortran pointer to imag data Afi2D")
      Afi2D => fpGetPi(mx)
      if( .not.associated(Afi2D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to associate pointer Afi2D")
      endif
      k = mexPrint("... The real data is:")
      call DisplayMatrixExplicit2(Afp2D)
      k = mexPrint("... The imag data is:")
      call DisplayMatrixExplicit2(Afi2D)
!\
! Access the data as a copy into a complex Fortran pointer. Since the
! MATLAB data is in two parts, one contiguous real part and another
! separate contiguous imag part, we cannot get a complex Fortran
! pointer directly into the data areas since Fortran stores the complex
! data as interleaved real & imag data. So we need to get a copy.
! Since this is a copy that uses dynamically allocated memory, we will
! need to deallocate it when we are done with it.
!/
      k = mexPrint("... Getting 2D pointer to complex copy Bfz2D")
      Bfz2D => fpGetPzCopy(mx)
      if( .not.associated(Bfz2D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to associate pointer Bfz2D")
      endif
      k = mexPrint("... The complex data is:")
      call DisplayMatrixExplicit2z(Bfz2D)
      k = mexPrint("... Deallocating complex copy")
      call fpDeallocate(Bfz2D)
!\
! Now lets allocate a Fortran pointer using the MATLAB API mxMalloc function
! in the background, then fill it with some data in a single assignment
! statement and print it out. Since the memory is dynamically allocated, we
! will need to deallocate it when we are done with it.
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint("... Allocate for complex Fortran pointer, Bfz2D")
      n1 = size(Afp2D,1)
      n2 = size(Afp2D,2)
      Bfz2D => fpAllocateZ(n1,n2)
      if( .not.associated(Bfz2D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to allocate memory for Bfz2D")
      endif
      k = mexPrint("... It worked, since Bfz2D is now associated")
      k = mexPrint("... Reshape it as a 1D matrix")
      n1 = size(Bfz2D)
      Cfz1D => fpReshape(Bfz2D,n1)
      if( .not.associated(Cfz1D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to reshape Bfz2D")
      endif
      k = mexPrint("... Filling Cfp1D with complex data")
      Cfz1D = (/ (i*(1.d0,-1.d0), i=1,size(Cfz1D)) /)
      k = mexPrint("... The complex data is:")
      call DisplayMatrixExplicit2z(Bfz2D)
      k = mexPrint("... Deallocating complex pointer")
      call fpDeallocate(Bfz2D)
!\
! Now try some reshapes on the Fortran pointers. Note that no data copying
! is involved ... we simply treat the underlying memory as any shape we
! want as long as the total size is the same and the memory has constant
! stride.
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint("... Getting 2D pointer to complex copy Bfz2D")
      Bfz2D => fpGetPzCopy(mx)
      if( .not.associated(Bfz2D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to associate pointer Bfz2D")
      endif
      k = mexPrint("... Reshaping Bfz2D as a 6 x 2 Cfz2D, no data copy")
      n1 = 6
      n2 = 2
      Cfz2D => fpReshape(Bfz2D,n1,n2)
      if( .not.associated(Cfz2D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to reshape Bfz2D as 6 x 2")
      endif
      k = mexPrint("... Calling the explicit print routine with Cfz2D")
      call DisplayMatrixExplicit2z(Cfz2D)
      k = mexPrint(" ")

      k = mexPrint("... Reshaping Bfz2D as a 4 x 3 Cfz2D, no data copy")
      n1 = 4
      n2 = 3
      Cfz2D => fpReshape(Bfz2D,n1,n2)
      if( .not.associated(Cfz2D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to reshape Cfz2D as 4 x 3")
      endif
      k = mexPrint("... Calling the explicit print routine with Cfz2D")
      call DisplayMatrixExplicit2z(Cfz2D)
      k = mexPrint(" ")

      k = mexPrint("... Reshaping Bfz2D as a 2 x 3 x 2 Cfz3D, no copy")
      n1 = 2
      n2 = 3
      n3 = 2
      Cfz3D => fpReshape(Bfz2D,n1,n2,n3)
      if( .not.associated(Cfz3D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to reshape Bfz2D as 2 x 3 x 2")
      endif
      k = mexPrint("... Calling the explicit print routine with Cfz3D")
      call DisplayMatrixExplicit3z(Cfz3D)

      k = mexPrint("... Deallocating complex copy Bfz2D")
      call fpDeallocate(Bfz2D)
!\
! Now try some reshapes directly on a matrix. Note that the input to
! these reshape routines are treated as regular arrays, not pointers.
! That means they can be used with regular Fortran variables and are
! not restricted to working with only Fortran pointers or MATLAB
! allocated memory.
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint("... Fortran allocate B as a 4 x 6")
      allocate(B(4,6),stat=istat)
      if( istat /= 0 ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to allocate B")
      endif
      k = mexPrint("... Getting a 1D pointer to B, Cfz1D")
      n1 = size(B)
      Cfz1D => fpReshape(B,n1)
      k = mexPrint("... Filling complex matrix B with natural numbers")
      Cfz1D = (/ (i*(1.d0,-1.d0), i=1,size(Cfz1D)) /)
      k = mexPrint("... Calling the explicit print routine with B")
      call DisplayMatrixExplicit2z(B)
      k = mexPrint(" ")

      k = mexPrint("... Reshaping B as a 3 x 8 Cfz2D, no data copy")
      n1 = 3
      n2 = 8
      Cfz2D => fpReshape(B,n1,n2)
      if( .not.associated(Cfz2D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to reshape Bfz2D as 3 x 8")
      endif
      k = mexPrint("... Calling the explicit print routine with Cfz2D")
      call DisplayMatrixExplicit2z(Cfz2D)
      k = mexPrint(" ")

      k = mexPrint("... Reshaping B as a 2 x 3 x 4 Cfz3D, no data copy")
      n1 = 2
      n2 = 3
      n3 = 4 
      Cfz3D => fpReshape(B,n1,n2,n3)
      if( .not.associated(Cfz3D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to reshape B as 2 x 3 x 4")
      endif
      k = mexPrint("... Calling the explicit print routine with Cfz3D")
      call DisplayMatrixExplicit3z(Cfz3D)
      k = mexPrint(" ")

      k = mexPrint("... Reshaping Cfz3D as a 3 x 4 x 2 Cfz3D, no copy")
      n1 = 3
      n2 = 4
      n3 = 2
      Cfz3D => fpReshape(Cfz3D,n1,n2,n3)
      if( .not.associated(Cfz3D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to reshape Cfz3D as 3 x 4 x 2")
      endif
      k = mexPrint("... Calling the explicit print routine with Cfz3D")
      call DisplayMatrixExplicit3z(Cfz3D)
      k = mexPrint(" ")

      k = mexPrint("... Reshaping Cfz1D as non-contiguous slice")
      n1 = 3
      n2 = 2
      n3 = 2
      Cfz3D => fpReshape(Cfz1D(1:size(Cfz1D):2),n1,n2,n3)
      if( .not.associated(Cfz3D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to reshape Cfz1D as 3 x 2 x 2")
      endif
      k = mexPrint("... Calling the explicit print routine with Cfz3D")
      call DisplayMatrixExplicit3z(Cfz3D)
      k = mexPrint(" ")

      k = mexPrintf("... Creating return mxArray from Cfz3D")
      k = mexPrint(" (the return variable should match above matrix)")
      plhs(1) = mxArray(Cfz3D)

      k = mexPrint("... Deallocating regular Fortran matrix B")
      deallocate(B)
!\
! Done with created mxArray mx
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint(" ")
      k = mexPrint("... Done")
      k = mexPrint(" ")
      call mxDestroyArray(mx)
      return
!\
! All of the contained routines have explicit interfaces and can use
! assumed shape arguments.
!/
      contains
!----------------------------------------------------------------------
      subroutine DisplayMatrixExplicit2(A)
      use MatlabAPImex
      implicit none
!-ARG
      real(8), intent(in) :: A(:,:)  ! Assumed shape A
!-LOC
      mwSize M, N
      character(len=500) line
      integer(4) i, k
      mwPointer address
!-----
      k = mexPrint("Explicit Interface 2D Matrix Print")
      M = size(A,1)
      N = size(A,2)
      if( M*N == 0 ) return
      address = loc(A(1,1))
      write(line,'(1X,A,Z16)') 'Address of data = ',address
      k = mexPrint(line)
      do i=1,M
          write(line,*) 'Row',i,' = ',int(A(i,:),1)
          k = mexPrint(line)
      enddo
      return
      end subroutine DisplayMatrixExplicit2
!----------------------------------------------------------------------
      subroutine DisplayMatrixExplicit2z(A)
      use MatlabAPImex
      implicit none
!-ARG
      complex(8), intent(in) :: A(:,:)  ! Assumed shape A
!-LOC
      mwSize M, N
      character(len=500) line
      integer(4) i, k
      mwPointer address
!-----
      k = mexPrint("Explicit Interface 2D Complex Matrix Print")
      M = size(A,1)
      N = size(A,2)
      if( M*N == 0 ) return
      address = loc(A(1,1))
      write(line,'(1X,A,Z16)') 'Address of data = ',address
      k = mexPrint(line)
      do i=1,M
          write(line,*) 'Row',i,' = ',A(i,:)
          k = mexPrint(line)
      enddo
      return
      end subroutine DisplayMatrixExplicit2z
!----------------------------------------------------------------------
      subroutine DisplayMatrixExplicit3(A)
      use MatlabAPImex
      implicit none
!-ARG
      real(8), intent(in) :: A(:,:,:)  ! Assumed shape A
!-LOC
      mwSize M, N, P
      character(len=500) line
      integer(4) i, j, k
      mwPointer address
!-----
      k = mexPrint("Explicit Interface 3D Matrix Print")
      M = size(A,1)
      N = size(A,2)
      P = size(A,3)
      if( M*N*P == 0 ) return
      address = loc(A(1,1,1))
      write(line,'(1X,A,Z16)') 'Address of data = ',address
      k = mexPrint(line)
      do j=1,p
          write(line,*) 'Sub-Matrix',j
          k = mexPrint(line)
          do i=1,M
              write(line,*) 'Row',i,' = ',int(A(i,:,j),1)
              k = mexPrint(line)
          enddo
      enddo
      return
      end subroutine DisplayMatrixExplicit3
!----------------------------------------------------------------------
      subroutine DisplayMatrixExplicit3z(A)
      use MatlabAPImex
      implicit none
!-ARG
      complex(8), intent(in) :: A(:,:,:)  ! Assumed shape A
!-LOC
      mwSize M, N, P
      character(len=500) line
      integer(4) i, j, k
      mwPointer address
!-----
      k = mexPrint("Explicit Interface 3D Matrix Print")
      M = size(A,1)
      N = size(A,2)
      P = size(A,3)
      if( M*N*P == 0 ) return
      address = loc(A(1,1,1))
      write(line,'(1X,A,Z16)') 'Address of data = ',address
      k = mexPrint(line)
      do j=1,p
          write(line,*) 'Sub-Matrix',j
          k = mexPrint(line)
          do i=1,M
              write(line,*) 'Row',i,' = ',A(i,:,j)
              k = mexPrint(line)
          enddo
      enddo
      return
      end subroutine DisplayMatrixExplicit3z
!----------------------------------------------------------------------
      end subroutine mexFunction
