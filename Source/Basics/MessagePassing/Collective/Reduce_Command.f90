!-- This module defines wrappers for the MPI reduction routines, providing
!   a generic Reduce() subroutine by overloading it for different data types,
!   rank, and the receiver of the reduction operation. 

module Reduce_Command
  
  use VariableManagement
  use Display
  use MessagePassingBasics

  implicit none
  private

  include 'mpif.h'

  public :: &
    Reduce

  interface Reduce
    module procedure ReduceReal
    module procedure ReduceReal_1D
    module procedure AllReduceReal
    module procedure AllReduceReal_1D
  end interface Reduce

contains

  subroutine ReduceReal(SourceData, C, Operation, TargetRank, TargetData)
    
    !-- Convention on argument ordering violated for first argument to 
    !   be the data being reduced, to make the call more readable.

    real(KR), intent(in) :: &
      SourceData
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KI), intent(in) :: &
      Operation, &
      TargetRank
    real(KR), intent(out) :: &
      TargetData
    
    integer(KI) :: &
      RealSize, &
      KindRealSize, &
      SizeRatio, &
      Count, &
      Error
    
    inquire(iolength=KindRealSize) 1.0_KR
    inquire(iolength=RealSize) 1.0
    
    SizeRatio = max(1, KindRealSize/RealSize)
    Count = 1
    
    if(mod(KindRealSize, RealSize) /= 0)then
      call Show('Mod(KindRealSize, RealSize) /= 0', CONSOLE_ERROR)
      call Abort(C)
    end if
    
    if(SizeRatio == 2)then
      call MPI_REDUCE( &
             SourceData, TargetData, Count, MPI_DOUBLE_PRECISION, Operation, &
             TargetRank, C%Handle, Error)
    else if(SizeRatio == 1)then
      call MPI_REDUCE( &
             SourceData, TargetData, Count, MPI_REAL, Operation, TargetRank, &
             C%Handle, Error)
    else
      call Show( &
             'RealSize does not correspond to any MPI Datatype', CONSOLE_ERROR)
      call Abort(C)
    end if   
      
  end subroutine ReduceReal


  subroutine ReduceReal_1D(SourceData, C, Operation, TargetRank, TargetData)
    
    !-- Convention on argument ordering violated for first argument to 
    !   be the data being reduced, to make the call more readable.

    real(KR), dimension(:), intent(in) :: &
      SourceData
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KI), intent(in) :: &
      Operation, &
      TargetRank
    real(KR), dimension(:), intent(out) :: &
      TargetData
    
    integer(KI) :: &
      RealSize, &
      KindRealSize, &
      SizeRatio, &
      Count, &
      Error
    
    inquire(iolength=KindRealSize) 1.0_KR
    inquire(iolength=RealSize) 1.0
    
    SizeRatio = max(1, KindRealSize/RealSize)
    Count = size(SourceData)
    
    if(mod(KindRealSize, RealSize) /= 0)then
      call Show('Mod(KindRealSize, RealSize) /= 0', CONSOLE_ERROR)
      call Abort(C)
    end if
    
    if(SizeRatio == 2)then
      call MPI_REDUCE( &
             SourceData, TargetData, Count, MPI_DOUBLE_PRECISION, Operation, &
             TargetRank, C%Handle, Error)
    else if(SizeRatio == 1)then
      call MPI_REDUCE( &
             SourceData, TargetData, Count, MPI_REAL, Operation, TargetRank, &
             C%Handle, Error)
    else
      call Show( &
             'RealSize does not correspond to any MPI Datatype', CONSOLE_ERROR)
      call Abort(C)
    end if   
      
  end subroutine ReduceReal_1D


  subroutine AllReduceReal(SourceData, C, Operation, TargetData)
    
    !-- Convention on argument ordering violated for first argument to 
    !   be the data being reduced, to make the call more readable.

    real(KR), intent(in) :: &
      SourceData
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KI), intent(in) :: &
      Operation
    real(KR), intent(out) :: &
      TargetData
    
    integer(KI) :: &
      RealSize, &
      KindRealSize, &
      SizeRatio, &
      Error
    
    inquire(iolength=KindRealSize) 1.0_KR
    inquire(iolength=RealSize) 1.0
    
    SizeRatio = max(1, KindRealSize/RealSize)
    
    if(mod(KindRealSize, RealSize) /= 0)then
      call Show('Mod(KindRealSize, RealSize) /= 0', CONSOLE_ERROR)
      call Abort(C)
    end if
    
    if(SizeRatio == 2)then
      call MPI_ALLREDUCE( &
             SourceData, TargetData, 1, MPI_DOUBLE_PRECISION, Operation, &
             C%Handle, Error)
    else if(SizeRatio == 1)then
      call MPI_ALLREDUCE( &
             SourceData, TargetData, 1, MPI_REAL, Operation, &
             C%Handle, Error)
    else
      call Show( &
             'RealSize does not correspond to any MPI Datatype', CONSOLE_ERROR)
      call Abort(C)
    end if   
      
  end subroutine AllReduceReal


  subroutine AllReduceReal_1D(SourceData, C, Operation, TargetData)
    
    !-- Convention on argument ordering violated for first argument to 
    !   be the data being reduced, to make the call more readable.

    real(KR), dimension(:), intent(in) :: &
      SourceData
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KI), intent(in) :: &
      Operation
    real(KR), dimension(:), intent(out) :: &
      TargetData
    
    integer(KI) :: &
      RealSize, &
      KindRealSize, &
      SizeRatio, &
      Count, &
      Error
    
    inquire(iolength=KindRealSize) 1.0_KR
    inquire(iolength=RealSize) 1.0
    
    SizeRatio = max(1, KindRealSize/RealSize)
    Count = size(SourceData)
    
    if(mod(KindRealSize, RealSize) /= 0)then
      call Show('Mod(KindRealSize, RealSize) /= 0', CONSOLE_ERROR)
      call Abort(C)
    end if
    
    if(SizeRatio == 2)then
      call MPI_ALLREDUCE( &
             SourceData, TargetData, Count, MPI_DOUBLE_PRECISION, Operation, &
             C%Handle, Error)
    else if(SizeRatio == 1)then
      call MPI_ALLREDUCE( &
             SourceData, TargetData, Count, MPI_REAL, Operation, &
             C%Handle, Error)
    else
      call Show( &
             'RealSize does not correspond to any MPI Datatype', CONSOLE_ERROR)
      call Abort(C)
    end if   
      
  end subroutine AllReduceReal_1D


end module Reduce_Command
