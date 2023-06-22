!-- This module defines a class that loads and stores selected rows and 
!   columns of a VariableGroup data array into a contiguous data array. 

module PackedVariableGroup_Base
  
  use Specifiers
  use VariableGroup_Base
  
  implicit none
  private
  
  public :: &
    Initialize, &
    Load, &
    Store, &
    Finalize

  type, public :: PackedVariableGroupBase
    integer(KI) :: &
      nData      = 0, &
      nVariables = 0
    integer(KI), dimension(:), pointer :: &
      UnpackedIndex => null()
    real(KR), dimension(:,:), allocatable :: &
      Data
    type(VariableGroupBase) :: &
      VariableGroup
  end type PackedVariableGroupBase

  interface Initialize
    module procedure Initialize_PVG
    module procedure Initialize_PVG_From_UnpackedData
  end interface Initialize
  
  interface Load
    module procedure Load_PVG
  end interface Load
  
  interface Store
    module procedure Store_PVG
  end interface Store
    
  interface Finalize
    module procedure Finalize_PVG
  end interface Finalize
    
    private :: &
      LoadVariable, &
      StoreVariable

contains

  
  subroutine Initialize_PVG(PVG, VG, UnpackedIndex)

    type(PackedVariableGroupBase), intent(inout) :: &
      PVG
    type(VariableGroupBase), intent(in) :: &
      VG
    integer(KI), dimension(:), intent(in), target :: &
      UnpackedIndex

    PVG%nData      = size(UnpackedIndex)
    PVG%nVariables = VG%nVariables

    PVG%UnpackedIndex => UnpackedIndex
    
    allocate(PVG%Data(PVG%nData,PVG%nVariables))

    call Initialize(PVG%VariableGroup, VG)

  end subroutine Initialize_PVG


  subroutine Initialize_PVG_From_UnpackedData( &
               PVG, UnpackedData, UnpackedIndex, SelectedOption)

    type(PackedVariableGroupBase), intent(inout) :: &
      PVG
    real(KR), dimension(:,:), intent(in) :: &
      UnpackedData
    integer(KI), dimension(:), intent(in), target :: &
      UnpackedIndex
    integer(KI), dimension(:), intent(in), optional :: &
      SelectedOption

    PVG%nData = size(UnpackedIndex)

    if(present(SelectedOption))then
      PVG%nVariables = size(SelectedOption)
    else
      PVG%nVariables = size(UnpackedData, dim=2)
    end if

    PVG%UnpackedIndex => UnpackedIndex

    allocate(PVG%Data(PVG%nData,PVG%nVariables))

    call Initialize( &
           PVG%VariableGroup, DataOption = UnpackedData, &
           SelectedOption = SelectedOption)

  end subroutine Initialize_PVG_From_UnpackedData
  
  
  subroutine Load_PVG(PVG)
    
    type(PackedVariableGroupBase), intent(inout), target :: &
      PVG
      
    integer(KI) :: &
      iVariable
    type(VariableGroupBase), pointer :: &
      VG
    
    if(size(PVG%UnpackedIndex) == 0) return
    
    VG => PVG%VariableGroup

    do iVariable = 1, PVG%nVariables
      call LoadVariable( &
             PVG%Data(:,iVariable), VG%Data(:,VG%Selected(iVariable)), &
             PVG%UnpackedIndex, PVG%nData)
    end do

  end subroutine Load_PVG
  
  
  subroutine Store_PVG(PVG)
    
    type(PackedVariableGroupBase), intent(in), target :: &
      PVG
    
    integer(KI) :: &
      iVariable
    type(VariableGroupBase), pointer :: &
      VG
    
    if(size(PVG%UnpackedIndex) == 0) return
    
    VG => PVG%VariableGroup

    do iVariable = 1, PVG%nVariables
      call StoreVariable( &
             VG%Data(:,VG%Selected(iVariable)), PVG%Data(:,iVariable), &
             PVG%UnpackedIndex, PVG%nData)
    end do
  
  end subroutine Store_PVG


  subroutine Finalize_PVG(PVG)

    type(PackedVariableGroupBase), intent(inout) :: &
      PVG

    call Finalize(PVG%VariableGroup)
    
    if(allocated(PVG%Data)) deallocate(PVG%Data)

    PVG%UnpackedIndex => null()

    PVG%nVariables = 0
    PVG%nData      = 0

  end subroutine Finalize_PVG
  
  
  subroutine LoadVariable( &
               PackedData, UnpackedData, UnpackedIndex, nData)
      
    real(KR), dimension(:), intent(inout) :: &
      PackedData
    real(KR), dimension(:), intent(in) :: &
      UnpackedData
    integer(KI), dimension(:), intent(in) :: &
      UnpackedIndex
    integer(KI), intent(in) :: &
      nData
    
    integer(KI) :: &
      iDatum
    
    do iDatum = 1, nData
      PackedData(iDatum) = UnpackedData(UnpackedIndex(iDatum))
    end do

  end subroutine LoadVariable


  subroutine StoreVariable( &
               UnpackedData, PackedData, UnpackedIndex, nData)
      
    real(KR), dimension(:), intent(inout) :: &
      UnpackedData
    real(KR), dimension(:), intent(in) :: &
      PackedData
    integer(KI), dimension(:), intent(in) :: &
      UnpackedIndex
    integer(KI), intent(in) :: &
      nData
    
    integer(KI) :: &
      iDatum
    
    do iDatum = 1, nData
      UnpackedData(UnpackedIndex(iDatum)) = PackedData(iDatum)
    end do

  end subroutine StoreVariable
  

end module PackedVariableGroup_Base
