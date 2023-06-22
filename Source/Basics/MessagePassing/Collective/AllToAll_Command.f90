!-- This module defines wrappers for the MPI all-to-all routine, providing
!   a generic AllToAll() subroutine by overloading it for different data types

module AllToAll_Command

  use VariableManagement
  use Display
  use MessagePassingBasics
  
  implicit none
  private
  
  include 'mpif.h'
  
  public :: &
    AllToAll
  
  interface AllToAll
    module procedure AllToAllReal
    module procedure AllToAllComplex
  end interface AllToAll
  
contains


  subroutine AllToAllReal(SendBuffer, C, ChunkSize, ReceiveBuffer)
    
    !-- Convention on argument ordering violated for first argument to 
    !   be the data being all-to-all'd, to make the call more readable.

    real(KR), dimension(:), intent(in) :: &
      SendBuffer
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KI), intent(in) :: &
      ChunkSize
    real(KR), dimension(:), intent(out) :: &
      ReceiveBuffer
    
    integer(KI) :: &
      RealSize, &
      KindRealSize, &
      SizeRatio, &
      Error
    
    if(C%Size == 1)then
      ReceiveBuffer = SendBuffer
      return
    end if
      
    inquire(iolength=KindRealSize) 1.0_KR
    inquire(iolength=RealSize) 1.0
    
    SizeRatio = max(1, KindRealSize/RealSize)
    
    if(mod(KindRealSize, RealSize) /= 0)then
      call Show('Mod(KindRealSize, RealSize) /= 0', CONSOLE_ERROR)
      call Abort(C)
    end if
    
    call MPI_ALLTOALL( &
           SendBuffer, SizeRatio*ChunkSize, MPI_REAL, &
           ReceiveBuffer, SizeRatio*ChunkSize, MPI_REAL, &
           C%Handle, Error)

  end subroutine AllToAllReal


  subroutine AllToAllComplex(SendBuffer, C, ChunkSize, ReceiveBuffer)
    
    !-- Convention on argument ordering violated for first argument to 
    !   be the data being all-to-all'd, to make the call more readable.

    complex(KC), dimension(:), intent(in) :: &
      SendBuffer
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KI), intent(in) :: &
      ChunkSize
    complex(KC), dimension(:), intent(out) :: &
      ReceiveBuffer
    
    integer(KI) :: &
      RealSize, &
      KindComplexSize, &
      SizeRatio, &
      Error
    complex(KC) :: &
      ComplexNumber
    
    if(C%Size == 1)then
      ReceiveBuffer = SendBuffer
      return
    end if
      
    ComplexNumber = (1.0,1.0)
    inquire(iolength=KindComplexSize) ComplexNumber
    inquire(iolength=RealSize) 1.0
    
    SizeRatio = max(1, KindComplexSize/RealSize)
    
    if(mod(KindComplexSize, RealSize) /= 0)then
      call Show('Mod(KindComplexSize, RealSize) /= 0', CONSOLE_ERROR)
      call Abort(C)
    end if
    
    call MPI_ALLTOALL( &
           SendBuffer, SizeRatio*ChunkSize, MPI_REAL, &
           ReceiveBuffer, SizeRatio*ChunkSize, MPI_REAL, &
           C%Handle, Error)

  end subroutine AllToAllComplex
  

end module AllToAll_Command
