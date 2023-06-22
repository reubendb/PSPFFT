!-- This module defines wrappers for the MPI non-blocking receive routines, 
!   providing a generic Receive() subroutine by overloading it for different 
!   data types.

module Receive_Command

  use VariableManagement
  use Display
  use MessagePassingBasics

  implicit none
  private
  
  include 'mpif.h'
  
  public :: &
    Receive
  
  interface Receive
    module procedure ReceiveInteger_1D
    module procedure ReceiveBigInteger_1D
    module procedure ReceiveReal_2D
  end interface Receive
  
contains


  subroutine ReceiveInteger_1D(TargetData, C, SourceRank, Tag, Handle)
    
    !-- Convention on argument ordering violated for first argument to 
    !   be the data being received, to make the call more readable.

    integer(KI), dimension(:), intent(out) :: &
      TargetData
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KI), intent(in) :: &
      SourceRank, &
      Tag
    integer(KI), intent(out) :: &
      Handle
    
    integer(KI) :: &
      ReceiveCount, &
      Error
    
    ReceiveCount = size(TargetData)
    
    call MPI_IRECV( &
           TargetData, ReceiveCount, MPI_INTEGER, SourceRank, Tag, C%Handle, &
           Handle, Error)
    
  end subroutine ReceiveInteger_1D

  
  subroutine ReceiveBigInteger_1D(TargetData, C, SourceRank, Tag, Handle)
    
    !-- Convention on argument ordering violated for first argument to 
    !   be the data being received, to make the call more readable.

    integer(KIB), dimension(:), intent(out) :: &
      TargetData
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KI), intent(in) :: &
      SourceRank, &
      Tag
    integer(KI), intent(out) :: &
      Handle
    
    integer(KI) :: &
      IntegerSize, &
      KindIntegerBig, &
      SizeRatio, &
      ReceiveCount, &
      Error
    
    IntegerSize = 1
    inquire(iolength=KindIntegerBig) 1_KIB
    inquire(iolength=IntegerSize) IntegerSize
    
    SizeRatio = max(1, KindIntegerBig/IntegerSize)
    ReceiveCount = size(TargetData) * SizeRatio
    
    if(mod(KindIntegerBig, IntegerSize) /= 0)then
      call Show('Mod(KindIntegerBig, IntegerSize) /= 0', CONSOLE_ERROR)
      call Abort(C)
    end if
    
    call MPI_IRECV( &
           TargetData, ReceiveCount, MPI_INTEGER, SourceRank, Tag, C%Handle, &
           Handle, Error)
    
  end subroutine ReceiveBigInteger_1D

  
  subroutine ReceiveReal_2D(TargetData, C, SourceRank, Tag, Handle)
    
    !-- Convention on argument ordering violated for first argument to 
    !   be the data being received, to make the call more readable.

    real(KR), dimension(:,:), intent(out) :: &
      TargetData
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KI), intent(in) :: &
      SourceRank, &
      Tag
    integer(KI), intent(out) :: &
      Handle
    
    integer(KI) :: &
      RealSize, &
      KindRealSize, &
      SizeRatio, &
      ReceiveCount, &
      Error
    
    inquire(iolength=KindRealSize) 1.0_KR
    inquire(iolength=RealSize) 1.0
    
    SizeRatio = max(1, KindRealSize/RealSize)
    ReceiveCount = size(TargetData) * SizeRatio
    
    if(mod(KindRealSize, RealSize) /= 0)then
      call Show('Mod(KindRealSize, RealSize) /= 0', CONSOLE_ERROR)
      call Abort(C)
    end if
    
    call MPI_IRECV( &
           TargetData, ReceiveCount, MPI_REAL, SourceRank, Tag, C%Handle, &
           Handle, Error)
    
  end subroutine ReceiveReal_2D


end module Receive_Command
