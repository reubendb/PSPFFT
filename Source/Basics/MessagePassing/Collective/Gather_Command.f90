!-- This module defines wrappers for the MPI gather routines, providing
!   a generic Gather() subroutine by overloading it for different data types,
!   rank, and the receiver of gathered data.

module Gather_Command
  
  use VariableManagement
  use Display
  use MessagePassingBasics

  implicit none
  private

  include 'mpif.h'

  public :: &
    Gather

  interface Gather
    module procedure GatherInteger_0D_1D
    module procedure GatherV_Real_1D_1D
    module procedure GatherV_Real_2D_2D
    module procedure AllGatherInteger_0D_1D
    module procedure AllGatherInteger_1D_2D
    module procedure AllGatherInteger_2D_3D
    module procedure AllGatherBigInteger_1D_2D
  end interface Gather

contains


  subroutine GatherInteger_0D_1D(SourceData, C, TargetRank, TargetData)
    
    !-- Convention on argument ordering violated for first argument to 
    !   be the data being gathered, to make the call more readable.

    integer(KI), intent(in) :: &
      SourceData
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KI), intent(in) :: &
      TargetRank
    integer(KI), dimension(:), intent(out) :: &
      TargetData
    
    integer(KI) :: &
      Error
    
    call MPI_GATHER( &
           SourceData, 1, MPI_INTEGER, &
           TargetData, 1, MPI_INTEGER, TargetRank, &
           C%Handle, Error)
  
  end subroutine GatherInteger_0D_1D
  
  
  subroutine GatherV_Real_1D_1D( &
               SourceData, C, ReceiveCount, TargetRank, TargetData)
    
    !-- Convention on argument ordering violated for first argument to 
    !   be the data being gathered, to make the call more readable.

    real(KR), dimension(:), intent(in) :: &
      SourceData
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KI), dimension(:), intent(in) :: &
      ReceiveCount
    integer(KI), intent(in) :: &
      TargetRank
    real(KR), dimension(:), intent(out) :: &
      TargetData
    
    integer(KI) :: &
      iRank, &
      RealSize, &
      KindRealSize, &
      SizeRatio, &
      SendCount, &
      Error
    integer(KI), dimension(size(ReceiveCount)) :: &
      ScaledReceiveCount, &
      Displacement
    real(KR), dimension(1) :: &
      DummySourceData

    if(sum(ReceiveCount) == 0) return

    inquire(iolength=KindRealSize) 1.0_KR
    inquire(iolength=RealSize) 1.0
    
    SizeRatio = max(1, KindRealSize/RealSize)
    ScaledReceiveCount = ReceiveCount * SizeRatio
    SendCount = size(SourceData) * SizeRatio
    
    if(mod(KindRealSize, RealSize) /= 0)then
      call Show('Mod(KindRealSize, RealSize) /= 0', CONSOLE_ERROR)
      call Abort(C)
    end if

    Displacement(1) = 0
    if(C%Size > 1)then
      do iRank = 2, C%Size
        Displacement(iRank) = sum(ScaledReceiveCount(1:iRank-1))
      end do
    end if

    if(SendCount == 0)then
      DummySourceData = -1
      call MPI_GATHERV( &
             DummySourceData, SendCount, MPI_REAL, &
             TargetData, ScaledReceiveCount, Displacement, MPI_REAL, &
             TargetRank, C%Handle, Error)       
    else
      call MPI_GATHERV( &
             SourceData, SendCount, MPI_REAL, &
             TargetData, ScaledReceiveCount, Displacement, MPI_REAL, &
             TargetRank, C%Handle, Error)
    end if

  end subroutine GatherV_Real_1D_1D

  
  subroutine GatherV_Real_2D_2D( &
               SourceData, C, ReceiveCount, TargetRank, TargetData)
    
    !-- Convention on argument ordering violated for first argument to 
    !   be the data being gathered, to make the call more readable.

    real(KR), dimension(:,:), intent(in) :: &
      SourceData
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KI), dimension(:), intent(in) :: &
      ReceiveCount
    integer(KI), intent(in) :: &
      TargetRank
    real(KR), dimension(:,:), intent(out) :: &
      TargetData
    
    integer(KI) :: &
      iRank, &
      iVariable, &
      nVariables, &
      oCell, &
      oData, &
      RealSize, &
      KindRealSize, &
      SizeRatio, &
      SendCount, &
      Error
    integer(KI), dimension(size(ReceiveCount)) :: &
      ScaledReceiveCount, &
      Displacement
    real(KR), dimension(size(TargetData)) :: &
      PackedTargetData
    
    inquire(iolength=KindRealSize) 1.0_KR
    inquire(iolength=RealSize) 1.0
    
    SizeRatio = max(1, KindRealSize/RealSize)
    ScaledReceiveCount = ReceiveCount * SizeRatio
    SendCount = size(SourceData) * SizeRatio
    
    if(mod(KindRealSize, RealSize) /= 0)then
      call Show('Mod(KindRealSize, RealSize) /= 0', CONSOLE_ERROR)
      call Abort(C)
    end if

    nVariables = size(SourceData, dim=2)
    Displacement(1) = 0
    if(C%Size > 1)then
      do iRank = 2, C%Size
        Displacement(iRank) = sum(ScaledReceiveCount(1:iRank-1)) * nVariables
      end do
    end if

    call MPI_GATHERV( &
           SourceData, SendCount, MPI_REAL, &
           PackedTargetData, ScaledReceiveCount*nVariables, Displacement, &
           MPI_REAL, TargetRank, C%Handle, Error)
    
    if(C%Rank == TargetRank)then
      Displacement = Displacement / sizeRatio
      do iRank = 1, C%Size
        oCell = Displacement(iRank) / nVariables
        do iVariable = 1, nVariables
          oData = Displacement(iRank) + (iVariable-1)*ReceiveCount(iRank)
          TargetData(oCell+1:oCell+ReceiveCount(iRank),iVariable) &
            = PackedTargetData(oData+1:oData+ReceiveCount(iRank))
        end do
      end do
    end if
      
  end subroutine GatherV_Real_2D_2D
  
  
  subroutine AllGatherInteger_0D_1D(SourceData, C, TargetData)

    !-- Convention on argument ordering violated for first argument to 
    !   be the data being gathered, to make the call more readable.

    integer(KI), intent(in) :: &
      SourceData
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KI), dimension(:), intent(out) :: &
      TargetData

    integer(KI) :: &
      Error

    call MPI_ALLGATHER( &
           SourceData, 1, MPI_INTEGER, &
           TargetData, 1, MPI_INTEGER, C%Handle, Error)

  end subroutine AllGatherInteger_0D_1D


  subroutine AllGatherInteger_1D_2D(SourceData, C, TargetData)

    !-- Convention on argument ordering violated for first argument to 
    !   be the data being gathered, to make the call more readable.

    integer(KI), dimension(:), intent(in) :: &
      SourceData
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KI), dimension(:,:), intent(out) :: &
      TargetData

    integer(KI) :: &
      Error

    call MPI_ALLGATHER( &
           SourceData, size(SourceData), MPI_INTEGER, &
           TargetData, size(SourceData), MPI_INTEGER, C%Handle, Error)

  end subroutine AllGatherInteger_1D_2D


  subroutine AllGatherInteger_2D_3D(SourceData, C, TargetData)

    !-- Convention on argument ordering violated for first argument to 
    !   be the data being gathered, to make the call more readable.

    integer(KI), dimension(:,:), intent(in) :: &
      SourceData
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KI), dimension(:,:,:), intent(out) :: &
      TargetData

    integer(KI) :: &
      Error

    call MPI_ALLGATHER( &
           SourceData, size(SourceData), MPI_INTEGER, &
           TargetData, size(SourceData), MPI_INTEGER, &
           C%Handle, Error)

  end subroutine  AllGatherInteger_2D_3D
  
  
  subroutine AllGatherBigInteger_1D_2D(SourceData, C, TargetData)
    
    !-- Convention on argument ordering violated for first argument to 
    !   be the data being gathered, to make the call more readable.

    integer(KIB), dimension(:), intent(in) :: &
      SourceData
    type(CommunicatorForm), intent(in) :: &
      C
    integer(KIB), dimension(:,:), intent(out) :: &
      TargetData
    
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
    
    call MPI_ALLGATHER( &
           SourceData, SendCount, MPI_INTEGER, &
           TargetData, SendCount, MPI_INTEGER, &
           C%Handle, Error)

  end subroutine AllGatherBigInteger_1D_2D


end module Gather_Command
