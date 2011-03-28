!*************************************************************************************
! 
!  MATLAB (R) is a trademark of The Mathworks (R) Corporation
! 
!  Function:    MatlabAPI_real
!  Filename:    MatlabAPI_real.f
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
!  for real matrices in the following files:
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
      real(8), allocatable :: B(:,:)
      real(8), pointer :: Afp2D(:,:)
      real(8), pointer :: Bfp2D(:,:)
      real(8), pointer :: Cfp3D(:,:,:)
      real(8), pointer :: Zfp1D(:)
      character(len=63), pointer :: names(:)
      mwPointer :: Apr
      mwSize :: M, N
      integer :: istat
      integer*4 :: i, j, k
      mwSize :: n1,n2, n3
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
! Generate a 2D double matrix for testing. Get a Fortran pointer to
! the real data and fill it with random numbers. Note that we are
! getting direct access to the data area of the mxArray variable mx
! through the use of the fpGetPr function. No data copying is involved.
! Also, the Fortran pointers can be used as regular Fortran arrays
! for all calculations in this routine. No need to use the messy %VAL()
! construct (that's buried behind the fpGetPr function). In this example,
! Afp2D and Zfp1D actually point to exactly the same memory, namely the
! real data area of the mxArray variable mx. The difference is that
! Afp2D treats the memory as a 2D matrix whereas Zfp1D treats the memory
! as a 1D matrix. The shape of the data area is automatically calculated
! inside the fpGetPr and gpGetPr1 functions and is a part of the Fortran
! pointer that is returned. Neat! For C programmers, note the difference
! here between Fortran pointers and C pointers. C pointers contain *only*
! a raw address and the type of object being pointed to (typically).
! Fortran pointers, however, always carry along with them not only the
! type being pointed to, but the shape and stride of the target. If you
! call a routine that expects a matrix as input, like the subroutines
! DisplayMatrixExplicit2 and DisplayMatrixImplicit2, then the address
! of the target will automatically be passed. I.e., it is like the
! pointer gets automatically dereferenced everywhere it is used as an
! array instead of a pointer. Nice!
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint("MatlabAPI_real test function")
      k = mexPrint(" ")
      k = mexPrint("... Generating a 3 x 4 double matrix for testing")
      n1 = 3
      n2 = 4
      mx = mxCreateDoubleMatrix(n1, n2, mxREAL)
      k = mexPrint("... Getting 2D Fortran pointer to real data Afp2D")
      Afp2D => fpGetPr(mx)
      if( .not.associated(Afp2D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to associate pointer Afp2D")
      endif
      k = mexPrint("... Getting 1D Fortran pointer to real data Zfp1D")
      Zfp1D => fpGetPr1(mx)
      if( .not.associated(Zfp1D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to associate pointer Zfp1D")
      endif
      k = mexPrint("... Filling real data Zfp1D with natural numbers")
      Zfp1D = (/ (i, i=1,size(Zfp1D)) /)
!\
! Access the input matrix using the %val() construct. No data copying is
! involved, but you cannot access the data as a matrix in this routine.
! Instead, you have to pass the value of the pointer to another routine
! using an implicit interface, and then that routine can access the data
! as a regular Fortran matrix. We will also have to get and then pass
! the M and N sizes of the matrix manually as well. This is the "old"
! F77 way of doing things and is shown in this routine for comparison
! only. This is NOT the way to do things with the new Fortran pointer
! routines in the MatlabAPImx module. See the previous section.
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint("... Getting mwPointer to the real data, Apr")
      Apr = mxGetPr(mx)
      k = mexPrint("... Getting the M and N sizes")
      M = mxGetM(mx)
      N = mxGetN(mx)
      k = mexPrint("... Calling the implicit print routine using Apr")
      call DisplayMatrixImplicit2(%val(Apr),M,N)
!\
! Access the input matrix by copying into an allocatable variable. Here you
! can access the data as a regular Fortran matrix in this routine and call
! other routines with explicit interfaces with this matrix as an argument.
! However, it requires a wasteful data copy. Also, the allocation uses the
! Fortran memory manager, not the MATLAB memory manager, so the MATLAB
! memory manager will know nothing about this memory and cannot garbage
! collect it. Again, this works but it is not the best way. This is kind
! of a mix of F95 features (use of an allocatable variable) and F77 features
! (the call to mxCopyPtrToReal8). The better way is to use the Fortran
! pointer routines demonstrated two sections up (fpGetPr function).
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint("... Getting mwPointer to the real data, Apr")
      Apr = mxGetPr(mx)
      k = mexPrint("... Getting the M and N sizes.")
      M = mxGetM(mx)
      N = mxGetN(mx)
      k = mexPrint("... Fortran allocate B as a 4 x 6")
      allocate(B(M,N),stat=istat)
      if( istat /= 0 ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to allocate B")
      endif
      k = mexPrint("... Copying data from Apr to Fortran array B")
      call mxCopyPtrToReal8(Apr,B,M*N)
      k = mexPrint("... Calling the explicit print routine with B")
      call DisplayMatrixExplicit2(B)
      k = mexPrint("... Deallocating B")
      deallocate(B)
!\
! Access the data through a Fortran pointer. This will allow us to access
! the underlying data as a regular Fortran matrix in this routine and use
! it as argument in routines with explicit interfaces. Note that the pointer
! carries with it the shape of the matrix, and passes this information along
! to any routine with assumed shape arguments. Nice!
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint("... Getting a Fortran pointer to real data, Afp2D")
      Afp2D => fpGetPr(mx)
      if( .not.associated(Afp2D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to associate pointer Afp2D")
      endif
      k = mexPrint("... Calling the explicit print routine with Afp2D")
      call DisplayMatrixExplicit2(Afp2D)
!\
! We can also pass the pointer to an implicit interface routine if we want,
! but in this case we will have to pass the sizes manually since the called
! routine cannot have assumed shape arguments.
!/
      k = mexPrint("... Calling the implicit print routine with Afp2D")
      call DisplayMatrixImplicit2(Afp2D,size(Afp2D,1),size(Afp2D,2))
!\
! Now lets allocate a Fortran pointer using the MATLAB API mxMalloc function
! in the background and then copy the data using the pointer to the original
! data. Note that the data copy takes place with an easy Bfp2D = Afp2D assignment.
! We don't have to use the clumsy mxCopyPtrToReal8 routine. Remember to
! deallocate the memory after we are done with it (the target data behind the
! Bfp2D pointer is dynamically allocated). Note that since this method uses the
! mxMalloc function in the background, the MATLAB memory manager knows about
! this dynamically allocated memory and can garbage collect it. Since the
! fpAllocate function returns a pointer, we need to use the => pointer
! assignment operator instead of the usual = assignment operator.
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint("... Allocate memory for Fortran pointer, Bfp2D")
      n1 = size(Afp2D,1)
      n2 = size(Afp2D,2)
      Bfp2D => fpAllocate(n1,n2)
      if( .not.associated(Bfp2D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to allocate memory for Bfp2D")
      endif
      k = mexPrint("... It worked, since Bfp2D is now associated")
      k = mexPrint("... Copying data from Afp2D to Bfp2D")
      Bfp2D = Afp2D
      k = mexPrint("... Calling the explicit print routine with Bfp2D")
      call DisplayMatrixExplicit2(Bfp2D)
      k = mexPrint("... Deallocating the memory associated with Bfp2D")
      call fpDeallocate(Bfp2D)
!\
! Now do the same thing we just did, but using the all-in-one fpGetCopy function.
! Note the use of the => pointer assignment operator here, since the fpGetCopy
! function returns a pointer. Don't forget to deallocate the memory when done
! with it. If you forget to use the => pointer assignment operator and instead
! use the regular = assignment operator, your program will bomb. That is
! because the statement Bfp2D = fpGetPrCopy(mx) means "Copy the data area of
! mx into the target that Bfp2D is currently pointing to". But since Bfp2D
! is null and does not have a valid target yet, this will attempt to copy into
! invalid memory and hence bomb your program. Bottom line: Always remember to
! use the => pointer assignment operator with the fpGet___ functions unless
! you really do want to do a data copy.
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint("... Allocate and copy into Fortran pointer Bfp2D")
      Bfp2D => fpGetPrCopy(mx)
      if( .not.associated(Bfp2D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to allocate memory for Bfp2D")
      endif
      k = mexPrint("... It worked, since Bfp2D is now associated")
      k = mexPrint("... Calling the explicit print routine with Bfp2D")
      call DisplayMatrixExplicit2(Bfp2D)
      k = mexPrint("... Deallocating the memory associated with Bfp2D")
      call fpDeallocate(Bfp2D)
!\
! Now try some reshapes on the Fortran pointers. Note that no data copying
! is involved ... we simply treat the underlying memory as any shape we
! want as long as the total size is the same and the memory has constant
! stride.
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint("... Reshaping Afp2D as a 6 x 2 Bfp2D, no data copy")
      n1 = 6
      n2 = 2
      Bfp2D => fpReshape(Afp2D,n1,n2)
      if( .not.associated(Bfp2D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to reshape Afp2D as 6 x 2")
      endif
      k = mexPrint("... Calling the explicit print routine with Bfp2D")
      call DisplayMatrixExplicit2(Bfp2D)
      k = mexPrint(" ")

      k = mexPrint("... Reshaping Bfp2D as a 4 x 3 Bfp2D, no data copy")
      n1 = 4
      n2 = 3
      Bfp2D => fpReshape(Bfp2D,n1,n2)
      if( .not.associated(Bfp2D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to reshape Bfp2D as 4 x 3")
      endif
      k = mexPrint("... Calling the explicit print routine with Bfp2D")
      call DisplayMatrixExplicit2(Bfp2D)
      k = mexPrint(" ")

      k = mexPrint("... Reshaping Bfp2D as a 2 x 3 x 2 Cfp3D, no copy")
      n1 = 2
      n2 = 3
      n3 = 2
      Cfp3D => fpReshape(Bfp2D,n1,n2,n3)
      if( .not.associated(Cfp3D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to reshape Bfp2D as 2 x 3 x 2")
      endif
      k = mexPrint("... Calling the explicit print routine with Cfp3D")
      call DisplayMatrixExplicit3(Cfp3D)
!\
! Now try some reshapes directly on a matrix. Note that the input to
! these reshape routines are treated as regular arrays, not pointers.
! That means they can be used with regular Fortran variables and are
! not restricted to working with only Fortran pointers or MATLAB
! allocated memory.
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint("... Allocating B as a 4 x 6")
      allocate(B(4,6),stat=istat)
      if( istat /= 0 ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to allocate B")
      endif
      k = mexPrint("... Getting a 1D pointer to B, Zfp1D")
      n1 = size(B)
      Zfp1D => fpReshape(B,n1)
      k = mexPrint("... Filling real matrix B with natural numbers")
      Zfp1D = (/ (i, i=1,size(Zfp1D)) /)
      k = mexPrint("... Calling the explicit print routine with B")
      call DisplayMatrixExplicit2(B)
      k = mexPrint(" ")

      k = mexPrint("... Reshaping B as a 3 x 8 Bfp2D, no data copy")
      n1 = 3
      n2 = 8
      Bfp2D => fpReshape(B,n1,n2)
      if( .not.associated(Bfp2D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to reshape Bfp2D as 3 x 8")
      endif
      k = mexPrint("... Calling the explicit print routine with Bfp2D")
      call DisplayMatrixExplicit2(Bfp2D)
      k = mexPrint(" ")

      k = mexPrint("... Reshaping B as a 2 x 3 x 4 Cfp3D, no data copy")
      n1 = 2
      n2 = 3
      n3 = 4
      Cfp3D => fpReshape(B,n1,n2,n3)
      if( .not.associated(Cfp3D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to reshape B as 2 x 3 x 4")
      endif
      k = mexPrint("... Calling the explicit print routine with Cfp3D")
      call DisplayMatrixExplicit3(Cfp3D)
      k = mexPrint(" ")

      k = mexPrint("... Reshaping Cfp3D as a 3 x 4 x 2 Cfp3D, no copy")
      n1 = 3
      n2 = 4
      n3 = 2
      Cfp3D => fpReshape(Cfp3D,n1,n2,n3)
      if( .not.associated(Cfp3D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to reshape Cfp3D as 3 x 4 x 2")
      endif
      k = mexPrint("... Calling the explicit print routine with Cfp3D")
      call DisplayMatrixExplicit3(Cfp3D)
      k = mexPrint(" ")

      k = mexPrint("... Reshaping Zfp1D as non-contiguous slice")
      n1 = 3
      n2 = 2
      n3 = 2
      Cfp3D => fpReshape(Zfp1D(1:size(Zfp1D):2),n1,n2,n3)
      if( .not.associated(Cfp3D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to reshape Zfp1D as 3 x 2 x 2")
      endif
      k = mexPrint("... Calling the explicit print routine with Cfp3D")
      call DisplayMatrixExplicit3(Cfp3D)
      k = mexPrint(" ")

      deallocate(B)
!\
! Now try to get the imaginary pointer to our purely real mxArray.
! This should fail because there is no actual imaginary memory.
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint("... Attempting to get an imag pointer of pure real")
      Afp2D => fpGetPi(mx)
      if( .not.associated(Afp2D) ) then
          k = mexPrint("... Good, it failed as expected")
      else
          k = mexPrint("... Not Good, internal error, contact author")
      endif
!\
! Now try to reshape a null pointer. Hopefully this will not work.
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint("... Attempting to reshape a null pointer")
      nullify(Afp2D)
      n1 = 4
      n2 = 0
      Bfp2D => fpReshape(Afp2D,n1,n2)
      if( .not.associated(Bfp2D) ) then
          k = mexPrint("... Good, it failed as expected")
      else
          k = mexPrint("... Not Good, internal error, contact author")
      endif
!\
! Now get a list of all the variable names in the workspace
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint("... Getting list of all workspace variable names:")
      names => fpMexGetNames( )
      if( associated(names) ) then
          do i=1,size(names)
              k = mexPrint(names(i))
          enddo
          call fpDeallocate1CharacterMex(names)
      else
          k = mexPrint("... No names found in workspace")
      endif
!\
! Now create an mxArray from one of our Fortran variables and return that
! to the MATLAB workspace.
!/
      k = mexPrint("--------------------------------------------------")
      k = mexPrint("... Allocating B as a 4 x 6")
      allocate(B(4,6),stat=istat)
      if( istat /= 0 ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to allocate B")
      endif
      k = mexPrint("... Getting 1D Fortran pointer to B Zfp1D")
      n1 = size(B)
      Zfp1D => fpReshape(B,n1)
      if( .not.associated(Zfp1D) ) then
          call mxDestroyArray(mx)
          call mexErrMsgTxt("Unable to associate pointer Zfp1D")
      endif
      k = mexPrint("... Filling real data Zfp1D with natural numbers")
      Zfp1D = (/ (i, i=1,size(Zfp1D)) /)
      k = mexPrint("... The returned variable should match this")
      call DisplayMatrixExplicit2(B)
      k = mexPrint("... Create mxArray directly from Fortran variable")
      plhs(1) = mxArray(B)
      k = mexPrint("... Deallocating B")
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
      character(len=200) line
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
      subroutine DisplayMatrixExplicit3(A)
      use MatlabAPImex
      implicit none
!-ARG
      real(8), intent(in) :: A(:,:,:)  ! Assumed shape A
!-LOC
      mwSize M, N, P
      character(len=200) line
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
      end subroutine mexFunction
!----------------------------------------------------------------------
!\
! The following routine, since it is not contained in another programming
! unit, has an implicit interface. As such, it cannot use assumed shape
! arguments and one is forced to pass the sizes manually.
!/
!----------------------------------------------------------------------
      subroutine DisplayMatrixImplicit2(A,M,N)
      use MatlabAPImex
      implicit none
!-ARG
      mwSize, intent(in) :: M, N
      real(8), intent(in) :: A(M,N)  ! Passed shape A
!-LOC
      character(len=200) line
      integer(4) i, k
      mwPointer address
!-----
      k = mexPrint("Implicit Interface 2D Matrix Print")
      if( M*N == 0 ) return
      address = loc(A(1,1))
      write(line,'(1X,A,Z16)') 'Address of data = ',address
      k = mexPrint(line)
      do i=1,M
          write(line,*) 'Row',i,' = ',int(A(i,:),1)
          k = mexPrint(line)
      enddo
      return
      end subroutine DisplayMatrixImplicit2
