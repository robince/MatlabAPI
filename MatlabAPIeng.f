!*************************************************************************************
! 
!  MATLAB (R) is a trademark of The Mathworks (R) Corporation
! 
!  Filename:    MatlabAPIeng.f
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
!  Interfaces included (see MATLAB doc for use):
!
!      engClose
!      engEvalString
!      engGetVariable
!      engOpen
!      engOutputBuffer
!      engPutVariable
!
!  Routines included (see comments below for use):
!
!      engCallMATLAB
!      engCreateLogicalSparseMatrix (works same as mxCreateLogicalSparseMatrix)
!      engGet  ! Works the same as mexGet but with Engine pointer
!      engSet  ! Works the same as mexSet but with Engine pointer
!      fpEngGetNames ! Gets list of all variable names in engine workspace
!-----
!
!  Change Log:
!  2009/Oct/27 --> Initial Release
!  2009/Dec/11 --> Changed default address function to LOC instead of %LOC
! 
!*************************************************************************************


#include "fintrf.h"

#ifndef mwPointer
#define mwPointer integer(4)
#endif

#ifndef mwSize
#define mwSize integer(4)
#endif

#ifndef mwIndex
#define mwIndex integer(4)
#endif

#ifdef PERCENTLOC
#define loc %LOC
#endif

      module MatlabAPIcharEng
      
      integer, parameter :: NameLengthMaxEng = 63
      
      contains

!----------------------------------------------------------------------
      function fpAllocate1CharacterEng(n) result(fp)
      implicit none
      character(len=NameLengthMaxEng), pointer :: fp(:)
!-ARG
      mwSize, intent(in) :: n
!-FUN
      mwPointer, external :: mxMalloc
!-COM
      character(len=NameLengthMaxEng), pointer :: Cpx1(:)
      common /MatlabAPI_COMCeng/ Cpx1
!-LOC
      mwPointer ptr
      mwSize, parameter :: NLMM = NameLengthMaxEng
!-----
      ptr = mxMalloc(NameLengthMaxEng * n)
      call MatlabAPI_COM_CpxEng(n, %val(ptr), %val(NLMM))
      fp => Cpx1
      return
      end function fpAllocate1CharacterEng
!----------------------------------------------------------------------
      subroutine fpDeallocate1CharacterEng(fp)
      implicit none
!-ARG
      character(len=NameLengthMaxEng), pointer :: fp(:)
!-LOC
      mwPointer ptr
!-----
      if( associated(fp) ) then
          ptr = loc(fp(1))
          call mxFree(ptr)
          nullify(fp)
      endif
      return
      end subroutine fpDeallocate1CharacterEng
!----------------------------------------------------------------------

      end module MatlabAPIcharEng

!----------------------------------------------------------------------

      subroutine MatlabAPI_COM_CpxEng(n, C)
      implicit none
!-PAR
      integer, parameter :: NameLengthMaxEng = 63
!-ARG
      mwSize, intent(in) :: n
      character(len=NameLengthMaxEng), target :: C(n)
!-COM
      character(len=NameLengthMaxEng), pointer :: Cpx1(:)
      common /MatlabAPI_COMCeng/ Cpx1
!-----
      Cpx1 => C
      return
      end subroutine MatlabAPI_COM_CpxEng

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------

      module MatlabAPIeng
      
      use MatlabAPIcharEng
      
!-----------------------------------------------------      
! Interface definitions for MATLAB API functions
!-----------------------------------------------------
      
      interface
!-----
      integer(4) function engClose(ep)
      mwPointer, intent(in) :: ep
      end function engClose
!-----
      integer(4) function engEvalString(ep, string)
      mwPointer, intent(in) :: ep
      character(len=*), intent(in) :: string
      end function engEvalString
!-----
      mwPointer function engGetVariable(ep, name)
      mwPointer, intent(in) :: ep
      character(len=*), intent(in) :: name
      end function engGetVariable
!-----

!engGetVisible (C)

!-----
      mwPointer function engOpen(startcmd)
      character(len=*),intent(in) :: startcmd
      end function engOpen
!-----

!engOpenSingleUse (C)

!-----
      integer(4) function engOutputBuffer(ep, p)
      mwPointer, intent(in) :: ep
      character(len=*), intent(in) :: p   ! Modified
      end function engOutputBuffer
!-----
      integer(4) function engPutVariable(ep, name, pm)
      mwPointer, intent(in) :: ep, pm
      character(len=*) name
      end function engPutVariable
!-----

!engSetVisible (C)

      end interface

!-----------------------------------------------------      

      contains

!----------------------------------------------------------------------
      

!*************************************************************************************
!*
!* Requires:    #include "fintrf.h"
!*
!* engCallMATLAB calls a MATLAB function using the MATLAB engine. This is the eng
!* equivalent of the function mexCallMATLAB for mex files. The engine pointer ep
!* must point to an engine that is already opened with the engOpen function.
!*
!* Inputs:  ep   = Engine pointer (mwPointer)
!*          nlhs = Number of desired output arguments (integer(4))
!*          nrhs = Number of input arguments (integer(4))
!*          prhs = Array of pointers to input mxArrays (mwPointer array)
!*          name = Name of the function to call (character*(*))
!*
!* Outputs: plhs = Array of pointers to output mxArrays (mwPointer array)
!*          returns 0 = success, 1 = failure
!*
!* Notes: This routine dynamically allocates memory for the return mxArray variables
!*        pointed to by the plhs array. It is up to the calling routine to destroy
!*        these arrays with the mxDestroyArray routine when done with them.
!*
!*        If engCallMATLAB returns a 1 for failure, then none of the plhs pointers
!*        will be set to anything, and there will be no need to destroy them.
!*
!*        The engCallMATLAB function works as follows:
!*        - Builds a command string with the appropriate number of dummy input
!*          names (ECMI00, ECMI01, ECMI02, ...) and dummy output names (ECMO00,
!*          ECMO01, ECMO02, ...).
!*        - Puts the inputs into the MATLAB workspace under the dummy input names.
!*        - Calls engEvalString to evaluate the command string.
!*        - Gets the resulting dummy outputs from the MATLAB workspace.
!*        - Clears all of the temporary variables (ECMI00, ...) from the MATLAB
!*          workspace.
!*        - If there is an error in any of the above processes, then all of the
!*          temporary mxArray variables are destroyed.
!*
!*        Because the temporary variables are put into the MATLAB workspace, this
!*        routine will only work if you do not have any existing MATLAB workspace
!*        variable names that clash with the dummy names that this routine uses.
!*        These names are:
!*          ECMInn, where nn = any two digit number from 00 - 99
!*          ECMOnn, where nn = any two digit number from 00 - 99
!*
!*        Example: To evaluate the equivalent of the following MATLAB function
!*
!*                   [a,b,c] = cart2pol[3,5,7)
!*
!*                 You would have to have these inputs:
!*
!*                   ep = pointer to Engine already opened with engOpen
!*                   nlhs = 3
!*                   plhs = array at least 3 in size
!*                   nrhs = 3
!*                   prhs(1) = pointer to mxArray containing the value 3
!*                   prhs(2) = pointer to mxArray containing the value 5
!*                   prhs(3) = pointer to mxArray containing the value 7
!*                   name = 'cart2pol'
!*
!*                 This routine will build the following command string:
!*
!*                   [ECMO00,ECMO01,ECMO02]=cart2pol(ECMI00,ECMI01,ECMI02);
!*
!*                 After putting ECMI00, ECMI01, and ECMI02 into the MATLAB
!*                 workspace, the engEvalString routine will be called with
!*                 the command string. Then the result variables ECMO00,
!*                 ECMO01, and ECMO02 will be gotten from the MATLAB workspace
!*                 and assigned to the plhs() array pointers. This routine will
!*                 then clear all of the MATLAB workspace variables for this
!*                 example:
!*
!*                   ECMO00, ECMO01, ECMO02, ECMI00, ECMI01, ECMI02
!*
!***********************************************************************************/

      integer(4) function engCallMATLAB(ep, nlhs, plhs, nrhs, prhs,name)
      implicit none
!-ARG
      mwPointer, intent(in)  :: ep         ! Really Engine *
      integer(4), intent(in) :: nlhs
      mwPointer, intent(out) :: plhs(*)    ! Really mxArray *[]
      integer(4), intent(in) :: nrhs
      mwPointer, intent(in)  :: prhs(*)    ! Really mxArray *[]
      character(len=*), intent(in) :: name
!-LOC
      integer, parameter :: limit = 500
      character(len=limit) command
      character(len=3) nn
      integer i, cj, k, inx, outx
!-----
!\
! Default result is failure
!/
      engCallMATLAB = 1
!\
! Check for proper numbers
!/
      if( nlhs < 0 .or. nrhs < 0 .or. nlhs > 99 .or. nrhs > 99 ) return
!\
! Constuct output string
!/
      if( 7 * nlhs + 2 > limit ) return
    
      if( nlhs == 0 ) then ! no output variables
          cj = 1
      elseif( nlhs == 1 ) then ! only one output variable
          command(1:7) = 'MCMO01='
          cj = 8
      else ! multiple outputs, enclose in brackets, comma separated
          command(1:1) = '['
          cj = 2
          do i=1,nlhs
              write(nn,'(i3)') 100+i
              command(cj:cj+6) = 'MCMO'//nn(2:3)//','
              cj = cj + 7
          enddo
          command(cj-1:cj) = ']='
          cj = cj + 1
      endif
!\
! Add the function name
!/
      if( cj > limit ) return
      command(cj:) = name
      do while( command(cj:cj) /= ' ' )
        cj = cj + 1
        if( cj > limit ) return
      enddo
!\
! Construct input string
!/
      if( nrhs /= 0 ) then
          if( cj + 7 * nrhs + 2 > limit ) return
          command(cj:cj) = '('
          cj = cj + 1
          do i=1,nrhs
              write(nn,'(i3)') 100+i
              command(cj:cj+6) = 'MCMI'//nn(2:3)//','
              cj = cj + 7
          enddo
        command(cj-1:cj-1) = ')'
      endif
!\
! Don't print results on MATLAB display, so add semi-colon
!/
      command(cj:cj) = ';'
!\
! Put the input variables into the MATLAB workspace
!/
      outx = 0
      do i=1,nrhs
          write(nn,'(i3)') 100+i
          k = engPutVariable( ep, 'MCMI'//nn(2:3), prhs(i) )
          if( k /= 0 ) then
              inx = i - 1
              goto 500
          endif
      enddo
      inx = nrhs
!\
! Evaluate the function in the MATLAB workspace
!/
      if( engEvalString( ep, command(1:cj) ) /= 0 ) goto 500
!\
! Get the output variables from the MATLAB workspace
!/
      do i=1,nlhs
          write(nn,'(i3)') 100+i
          plhs(i) = engGetVariable( ep, 'MCMO'//nn(2:3) )
          if( plhs(i) == 0 ) then
              outx = i - 1
              goto 500
          endif
      enddo
      outx = nlhs
!\
! Success
!/
      engCallMATLAB = 0
!\
! Clean up the MATLAB workspace and temporary mxArray variables
!/
  500 continue
      do i=1,inx
          write(nn,'(i3)') 100+i
          k = engEvalString( ep, 'clear MCMI'//nn(2:3)//';' )
      enddo
      do i=1,outx
          write(nn,'(i3)') 100+i
          k = engEvalString( ep, 'clear MCMO'//nn(2:3)//';' )
          if( engCallMATLAB /= 0 ) then
              call mxDestroyArray( plhs(i) )
              plhs(i) = 0
          endif
      enddo

      return

      end function engCallMATLAB

!------------------------------------------------------------------------------
!
! Function:    engCreateSparseLogicalMatrix
!
! Arguments
!
!  ep
!
!    The engine pointer to a previously opened MATLAB engine
!
!  m
!
!    The desired number of rows
!
!  n
!
!    The desired number of columns
!
!  nzmax
!
!    The number of elements that engCreateSparseLogicalMatrix should allocate to
!    hold the data. Set the value of nzmax to be greater than or equal to the
!    number of nonzero elements you plan to put into the mxArray, but make sure
!    that nzmax is less than or equal to m*n.
!
!  Returns
!
!    A pointer to the created mxArray, if successful. If unsuccessful in a
!    stand-alone (nonMEX-file) application, engCreateSparseLogicalMatrix returns
!    0. engCreateSparseLogicalMatrix is unsuccessful when there is not enough
!    free heap space to create the mxArray.
!
!  Description
!
!    Use engCreateSparseLogicalMatrix to create an m-by-n mxArray of mxLogical
!    elements. engCreateSparseLogicalMatrix initializes each element in the array
!    to logical 0.
!
!  Call mxDestroyArray when you finish using the mxArray. mxDestroyArray
!  deallocates the mxArray and its elements.
!
!******************************************************************************

!-----------------------------------------------------------------------------

      mwPointer function engCreateSparseLogicalMatrix(ep,m,n,nzmax)
      implicit none
!-ARG
      mwPointer, intent(in) :: ep     ! really Engine*
      mwSize, intent(in) :: m, n, nzmax
!-PAR
      mwSize, parameter :: sizeoflogical = 1  ! logicals in MATLAB are 1 byte
!-FUN
      mwPointer, external :: mxCalloc
      mwPointer, external :: mxGetData
      mwPointer, external :: mxGetIr
      mwPointer, external :: mxGetJc
      mwPointer, external :: mxMalloc
!-LOC
      integer(4) k
      mwPointer plhs(1)    ! really mxArray*
      mwPointer pr         ! really mxLogical*
      mwPointer jc, ir     ! really mwIndex*
      mwIndex mwx(2)       ! used for sizeof calculation
      mwSize sizeofindex
!-----
!\
! Default return value is failure
!/
      engCreateSparseLogicalMatrix = 0
!\
! Get size of the MATLAB type mwIndex
!/
      sizeofindex = loc(mwx(2)) - loc(mwx(1))
!\
! First create an empty sparse logical matrix in the engine. Use a variable
! name that hopfully will not clash with an existing name in the workspace.
!/
      k = engEvalString( ep, "mxCSLM_31415=logical(sparse([]))" )
      if( k /= 0 ) return
!\
! Get the sparse logical matrix from the engine
!/
      plhs(1) = engGetVariable( ep, "mxCSLM_31415" )
      k = engEvalString( ep, "clear mxCSLM_31415" )
      if( plhs(1) == 0 ) return
!\
! Allocate new memory for data and indexes based on nzmax.
! For engine applications we need to check the return values.
!/
      pr = mxMalloc( nzmax * sizeoflogical )  ! Not initialized
      if( pr == 0 ) then
        call mxDestroyArray( plhs(1) )
        return
      endif
      ir = mxMalloc( nzmax * sizeofindex )  ! Not initialized
      if( ir == 0 ) then
        call mxFree( pr )
        call mxDestroyArray( plhs(1) )
        return
      endif
      jc = mxCalloc( n+1, sizeofindex )  ! Initialized to all zero
      if( jc == 0 ) then
        call mxFree( ir )
        call mxFree( pr )
        call mxDestroyArray( plhs(1) )
        return
      endif
!\
! Free the current data and index memory
!/
      call mxFree( mxGetData( plhs(1) ) )
      call mxFree( mxGetIr( plhs(1) ) )
      call mxFree( mxGetJc( plhs(1) ) )
!\
! Reset the pointers to the new memory
!/
      call mxSetM( plhs(1), m )
      call mxSetN( plhs(1), n )
      call mxSetData( plhs(1), pr )
      call mxSetIr( plhs(1), ir )
      call mxSetJc( plhs(1), jc )
      call mxSetNzmax( plhs(1), nzmax )
!\
! Everything went ok, so set return value
!/
      engCreateSparseLogicalMatrix = plhs(1)

      return
      end function
      
!----------------------------------------------------------------------
 
      mwPointer function engGet(ep, handle, property)
      implicit none
!-ARG
      mwPointer, intent(in) :: ep     ! really Engine*
      real(8), intent(in) :: handle
      character(len=*), intent(in) :: property
!-FUN
      mwPointer, external :: mxCreateDoubleScalar
      mwPointer, external :: mxCreateString
!-LOC
      mwPointer mxproperty
      mwPointer mx(2), my(1)
      integer(4) trapflag
!-----
      mx(1) = mxCreateDoubleScalar(handle)
      mx(2) = mxCreateString(property)
      trapflag = engCallMATLAB(ep, 1, my, 2, mx, "get")
      call mxDestroyArray(mx(2))
      call mxDestroyArray(mx(1))
      if( trapflag == 0 ) then
          engGet = my(1)
      else
          engGet = 0
      endif
      return
      end function engGet
      
!----------------------------------------------------------------------
 
      integer(4) function engSet(ep, handle, property, value)
      implicit none
!-ARG
      mwPointer, intent(in) :: ep     ! really Engine*
      real(8), intent(in) :: handle
      character(len=*), intent(in) :: property
      mwPointer, intent(in) :: value
!-FUN
      mwPointer, external :: mxCreateDoubleScalar
      mwPointer, external :: mxCreateString
!-LOC
      mwPointer mx(3)
      mwPointer answer(1)
!-----
      mx(1) = mxCreateDoubleScalar(handle)
      mx(2) = mxCreateString(property)
      mx(3) = value
      engSet = engCallMATLAB(ep, 0, answer, 3, mx, "set")
      call mxDestroyArray(mx(2))
      call mxDestroyArray(mx(1))
      return
      end function engSet
      
!----------------------------------------------------------------------

      function fpEngGetNames(ep) result(fp)
      implicit none
      character(len=NameLengthMaxEng), pointer :: fp(:)
!-ARG
      mwPointer, intent(in) :: ep     ! really Engine*
!-FUN
      mwPointer, external :: mxGetField
      mwSize, external :: mxGetNumberOfElements
      integer*4, external :: mxGetString
!-LOC
      mwPointer rhs(1), lhs(1)
      mwPointer mx
      integer(4) k
      mwIndex i, n
!-----
      nullify(fp)
      k = engCallMATLAB(ep, 1, lhs, 0, rhs, "whos") ! Get list of variables
      if( k == 0 ) then
          n = mxGetNumberOfElements(lhs(1)) ! Get number of variables
          if( n /= 0 ) then
              fp => fpAllocate1CharacterEng(n) ! Allocate space for array
              if( associated(fp) ) then
                  do i=1,n
                      mx = mxGetField(lhs(1), i, "name") ! Get the name
                      k = mxGetString(mx, fp(i), NameLengthMaxEng) ! Copy into our array
                  enddo
              endif
          endif
          call mxDestroyArray(lhs(1)) ! Free the result of the whos call
      endif
      return
      end function fpEngGetNames
      
!----------------------------------------------------------------------

      end module MatlabAPIeng
