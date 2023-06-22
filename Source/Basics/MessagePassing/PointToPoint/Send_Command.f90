!-- This module defines wrappers for the MPI non-blocking send routines, 
!   providing a generic Send() subroutine by overloading it for different 
!   data types

module Send_Command

  use VariableManagement
  use Display
  use MessagePassingBasics

  implicit none
  private
  
  include 'mpif.h'
  
  public :: &
    Send
  
  interface Send
    module procedure SendInteger_1D
    module procedure SendBigInteger_1D
    module procedure SendReal_2D
  end interface Send
  
contains
  
  
  subroutine SendInteger_1D(SourceData, C, TargetRank, Tag, Handle)
    
    !-- Convention on argument ordering violated for first argument to 
    !   be the data being sent, to make the call more readable.

    integer(KI), dimension(:), intent(in) :: &
      SourceData
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KI), intent(in) :: &
      TargetRank, &
      Tag
    integer(KI), intent(out) :: &
      Handle
    
    integer(KI) :: &
      SendCount, &
      Error
    
    SendCount = size(SourceData)
    
    call MPI_ISEND( &
           SourceData, SendCount, MPI_INTEGER, TargetRank, Tag, C%Handle, &
           Handle, Error)
    
  end subroutine SendInteger_1D
  

  subroutine SendBigInteger_1D(SourceData, C, TargetRank, Tag, Handle)

    !-- Convention on argument ordering violated for first argument to 
    !   be the data being sent, to make the call more readable.

    integer(KIB), dimension(:), intent(in) :: &
      SourceData
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KI), intent(in) :: &
      TargetRank, &
      Tag
    integer(KI), intent(out) :: &
      Handle
    
    integer(KI) :: &
      IntegerSize, &
      KindIntegerBig, &
      SizeRatio, &
      SendCount, &
      Error
    
    IntegerSize = 1
    inquire(iolength=KindIntegerBig) 1_KIB
    inquire(iolength=IntegerSize) IntegerSize
    
    SizeRatio = max(1, KindIntegerBig/IntegerSize)
    SendCount = size(SourceData) * SizeRatio
    
    if(mod(KindIntegerBig, IntegerSize) /= 0)then
      call Show('Mod(KindIntegerBig, IntegerSize) /= 0', CONSOLE_ERROR)
      call Abort(C)
    end if
    
    call MPI_ISEND( &
           SourceData, SendCount, MPI_INTEGER, TargetRank, Tag, C%Handle, &
           Handle, Error)
    
  end subroutine SendBigInteger_1D
  
  
  subroutine SendReal_2D(SourceData, C, TargetRank, Tag, Handle)
    
    !-- Convention on argument ordering violated for first argument to 
    !   be the data being sent, to make the call more readable.

    real(KR), dimension(:,:), intent(in) :: &
      SourceData
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KI), intent(in) :: &
      TargetRank, &
      Tag
    integer(KI), intent(out) :: &
      Handle
    
    integer(KI) :: &
      RealSize, &
      KindRealSize, &
      SizeRatio, &
      SendCount, &
      Error
    
    inquire(iolength=KindRealSize) 1.0_KR
    inquire(iolength=RealSize) 1.0
    
    SizeRatio = max(1, KindRealSize/RealSize)
    SendCount = size(SourceData) * SizeRatio
    
    if(mod(KindRealSize, RealSize) /= 0)then
      call Show('Mod(KindRealSize, RealSize) /= 0', CONSOLE_ERROR)
      call Abort(C)
    end if

    call MPI_ISEND( &
           SourceData, SendCount, MPI_REAL, TargetRank, Tag, C%Handle, &
           Handle, Error)
    
  end subroutine SendReal_2D
  

end module Send_Command
