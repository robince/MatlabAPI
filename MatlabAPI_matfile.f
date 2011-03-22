!*************************************************************************************
! 
!  MATLAB (R) is a trademark of The Mathworks (R) Corporation
! 
!  Function:    MatlabAPI_matfile
!  Filename:    MatlabAPI_matfile.f
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
!  MatlabAPImat.f  - Interface definitions for the MATLAB API mex___ functions
!
!  Both files have extra routines that are not part of the original MATLAB API
!  function list. Some of the extra routines are drop-in replacements for original
!  MATLAB API functions that were not part of the Fortran API. Other routines are
!  brand new ... e.g. all of the Fortran pointer routines. See comments in these
!  files for details on the interface for these functions.
!
!  This routine opens a mat file, places some variables in it, gets a list of the
!  variables in it, then reads the variables.
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
      use MatlabAPImat
      implicit none
!-ARG
      mwPointer plhs(*), prhs(*)
      integer*4 nlhs, nrhs
!-LOC
      character(len=NameLengthMaxMex), pointer :: names(:)
      mwPointer mx, matfile
      integer*4 i, k
!-----
!\
! Check for no inputs or outputs.
!/
      if( nrhs /= 0 .or. nlhs /= 0 ) then
          call mexErrMsgTxt("This function has no inputs or outputs")
      endif
!\
! Open the mat file in write-only mode.
!/
      k = mexPrint("... Opening mat file MatlabAPI_matfile.mat")
      matfile = matOpen("MatlabAPI_matfile.mat","w")
      if( matfile == 0 ) then
          call mexErrMsgTxt("Unable to open mat file")
      endif
!\
! Put some variables in it.
!/
      k = mexPrint("... Putting some variables in the file")
      mx = mxCreateDoubleScalar(1.d0)
      k = matPutVariable(matfile, "FirstVariable", mx)
      k = matPutVariable(matfile, "SecondVariable", mx)
      k = matPutVariable(matfile, "ThirdVariable", mx)
      k = matPutVariable(matfile, "FourthVariable", mx)
      call mxDestroyArray(mx)
!\
! Close the file
!/
      k = mexPrint("... Closing the file")
      k = matClose(matfile)
!\
! Open the mat file in read-only mode.
!/
      k = mexPrint("... Opening mat file MatlabAPI_matfile.mat")
      matfile = matOpen("MatlabAPI_matfile.mat","r")
      if( matfile == 0 ) then
          call mexErrMsgTxt("Unable to open mat file")
      endif
!\
! Get a list of variable names in the mat file.
!/
      k = mexPrint("... List of variable names in the file:")
      names => fpMatGetNames(matfile)
      if( associated(names) ) then
          do i=1,size(names)
              k = mexPrint(names(i))
          enddo
      else
          call mexErrMsgTxt("Unable to read the variable names")
      endif
!\
! Go get the variables and print out some info
!/
      k = mexPrint("... Getting all the variables from the file")
      do i=1,size(names)
          mx = matGetVariable(matfile, names(i))
          if( mx /= 0 ) then
              k = mexPrint("Got the variable "//names(i))
              call mxDestroyArray(mx)
          else
              call mexErrMsgTxt("Unable to get variable "//names(i))
          endif
      enddo
      call fpDeallocate1CharacterMex(names)
!\
! Done.
!/
      k = mexPrint("... Done.")
      k = matClose(matfile)
      return

      end subroutine mexFunction
