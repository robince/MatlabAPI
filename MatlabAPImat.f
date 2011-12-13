!*************************************************************************************
! 
!  MATLAB (R) is a trademark of The Mathworks (R) Corporation
! 
!  Filename:    MatlabAPImat.f
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
!  Interfaces included (see MATLAB doc for use):
!
!      matClose
!      matDeleteVariable
!      matGetDir
!      matGetNextVariable
!      matGetNextVariableInfo
!      matGetVariable
!      matGetVariableInfo
!      matOpen
!      matPutVariable
!      matPutVariableAsGlobal
!
!  Routine included (see below for use)
!
!      fpMatGetNames
!-----
!
!  Change Log:
!  2009/Oct/27 --> Initial Release
!  2009/Dec/11 --> Changed default address function to LOC instead of %LOC
!  2011/Jan/03 --> Added include file fintrf.h
!
!*************************************************************************************

#include "fintrf.h"

#ifndef mwSize
#define mwSize integer(4)
#endif

#ifndef mwPointer
#define mwPointer integer(4)
#endif

#ifdef PERCENTLOC
#define loc %LOC
#endif

      module MatlabAPIcharMat
      
      integer, parameter :: NameLengthMaxMat = 63
      
      contains

!----------------------------------------------------------------------
      function fpAllocate1CharacterMat(n) result(fp)
      implicit none
      character(len=NameLengthMaxMat), pointer :: fp(:)
!-ARG
      mwSize, intent(in) :: n
!-FUN
      mwPointer, external :: mxMalloc
!-COM
      character(len=NameLengthMaxMat), pointer :: Cpx1(:)
      common /MatlabAPI_COMCmat/ Cpx1
!-LOC
      mwPointer ptr
      mwSize, parameter :: NLMM = NameLengthMaxMat
!-----
      nullify(fp)
      if( n > 0 ) then
          ptr = mxMalloc(NameLengthMaxMat * n)
          if( ptr /= 0 ) then
              call MatlabAPI_COM_CpxMat(n, %val(ptr), %val(NLMM))
              fp => Cpx1
          endif
      endif
      return
      end function fpAllocate1CharacterMat
!----------------------------------------------------------------------
      subroutine fpDeallocate1CharacterMat(fp)
      implicit none
!-ARG
      character(len=NameLengthMaxMat), pointer :: fp(:)
!-LOC
      mwPointer ptr
!-----
      if( associated(fp) ) then
          ptr = loc(fp(1))
          call mxFree(ptr)
          nullify(fp)
      endif
      return
      end subroutine fpDeallocate1CharacterMat

!----------------------------------------------------------------------

      end module MatlabAPIcharMat

!----------------------------------------------------------------------

      subroutine MatlabAPI_COM_CpxMat(n, C)
      implicit none
!-PAR
      integer, parameter :: NameLengthMaxMat = 63
!-ARG
      mwSize, intent(in) :: n
      character(len=NameLengthMaxMat), target :: C(n)
!-COM
      character(len=NameLengthMaxMat), pointer :: Cpx1(:)
      common /MatlabAPI_COMCmat/ Cpx1
!-----
      Cpx1 => C
      return
      end subroutine MatlabAPI_COM_CpxMat

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------

      module MatlabAPImat
      
      use MatlabAPIcharMat
      
!-----------------------------------------------------      
! Interface definitions for MATLAB API functions
!-----------------------------------------------------
      
      interface
!-----
      integer(4) function matClose(mfp)
      mwPointer, intent(in) :: mfp
      end function matClose
!-----
      integer(4) function matDeleteVariable(mfp, name)
      mwPointer, intent(in) :: mfp
      character(len=*), intent(in) :: name
      end function matDeleteVariable
!-----
      mwPointer function matGetDir(mfp, num)
      mwPointer, intent(in) :: mfp
      integer(4), intent(in) :: num
      end function matGetDir
!-----

!matGetFp (C)

!-----
      mwPointer function matGetNextVariable(mfp, name)
      mwPointer, intent(in) :: mfp
      character(len=*), intent(in) :: name
      end function matGetNextVariable
!-----
      mwPointer function matGetNextVariableInfo(mfp, name)
      mwPointer, intent(in) :: mfp
      character(len=*), intent(in) :: name
      end function matGetNextVariableInfo
!-----
      mwPointer function matGetVariable(mfp, name)
      mwPointer, intent(in) :: mfp
      character(len=*),intent(in) :: name
      end function matGetVariable
!-----
      mwPointer function matGetVariableInfo(mfp, name);
      mwPointer, intent(in) :: mfp
      character(len=*), intent(in) :: name
      end function matGetVariableInfo
!-----
      mwPointer function matOpen(filename, mode)
      character(len=*),intent(in) :: filename, mode
      end function matOpen
!-----
      integer(4) function matPutVariable(mfp, name, pm)
      mwPointer, intent(in) :: mfp, pm
      character(len=*), intent(in) :: name
      end function matPutVariable
!-----
      integer(4) function matPutVariableAsGlobal(mfp, name, pm)
      mwPointer, intent(in) :: mfp, pm
      character(len=*), intent(in) :: name
      end function matPutVariableAsGlobal
      
      end interface
      
!----------------------------------------------------------------------
      
      contains

!----------------------------------------------------------------------
      function fpMatGetNames(mfp) result(fp)
      implicit none
      character(len=NameLengthMaxMat), pointer :: fp(:)
!-ARG
      mwPointer, intent(in) :: mfp
!-COM
      mwPointer, pointer :: Ppx1(:)
      common /MatlabAPI_COMP/ Ppx1
!-LOC
      mwPointer dir
      integer(4) i, num
!-----
      dir = matGetDir(mfp, num)
      if( dir /= 0 ) then
          fp => fpAllocate1CharacterMat(num)
          if( associated(fp) ) then
              call MatlabAPI_COM_Ppx(%val(dir), num)
              do i=1,num
                  call mxCopyPtrToCharacter(Ppx1(i),fp(i),              &
     &                                      NameLengthMaxMat)
              enddo
          endif
      else
          nullify(fp)
      endif
      return
      end function fpMatGetNames

!----------------------------------------------------------------------

      end module MatlabAPImat

!----------------------------------------------------------------------

      subroutine MatlabAPI_COM_Ppx(P, n)
      implicit none
!-ARG
      mwSize, intent(in) :: n
      mwPointer, target :: P(n)
!-COM
      mwPointer, pointer :: Ppx1(:)
      common /MatlabAPI_COMP/ Ppx1
!-----
      Ppx1 => P
      return
      end subroutine MatlabAPI_COM_Ppx
