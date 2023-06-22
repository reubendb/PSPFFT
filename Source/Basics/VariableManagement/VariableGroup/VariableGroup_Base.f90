!-- This module defines a class that stores data for a group of variables,
!   together with some information about them (names, units, etc.). 

module VariableGroup_Base
  
  use Specifiers
  use Arrays
    
  implicit none
  private
  
  public :: &
    Initialize, &
    Finalize

  type, public :: VariableGroupBase
    integer(KI) :: &
      nData      = 0, &
      nVariables = 0, &
      nVectors   = 0, &
      lName      = 0
    integer(KI), dimension(:), allocatable :: &
      lVariable, &
      lVector, &
      Selected
    real(KR), dimension(:,:), pointer :: &
      Data => null()
    character(LL) :: &
      Name = ''
    character(LL), dimension(:), allocatable :: &
      Variable, &
      Vector
    logical(KL) :: &
      LocalDataStorage = .false.
    type(LabeledValueBase), dimension(:), allocatable :: &
      Unit
    type(ArrayInteger_1D_Base), dimension(:), allocatable :: &
      VectorIndices
  end type VariableGroupBase

  interface Initialize
    module procedure Initialize_VG
    module procedure Initialize_VG_From_VG
  end interface Initialize

  interface Finalize
    module procedure Finalize_VG
  end interface Finalize

contains

  
  subroutine Initialize_VG( &
               VG, VectorIndicesOption, UnitOption, VectorOption, &
               VariableOption, NameOption, DataOption, DataShapeOption, &
               SelectedOption)

    type(VariableGroupBase), intent(inout) :: &
      VG
    type(ArrayInteger_1D_Base), dimension(:), intent(in), optional ::&
      VectorIndicesOption
    type(LabeledValueBase), dimension(:), intent(in), optional :: &
      UnitOption
    character(*), dimension(:), intent(in), target, optional :: &
      VariableOption, &
      VectorOption
    character(*), intent(in), optional :: &
      NameOption
    real(KR), dimension(:,:), intent(in), target, optional :: &
      DataOption
    integer(KI), dimension(:), intent(in), optional :: &
      DataShapeOption, &
      SelectedOption
    
    integer(KI) :: &
      iVariable

    if(present(DataOption))then
      VG%nData = size(DataOption, dim=1)
    else if(present(DataShapeOption))then
      VG%nData = DataShapeOption(1)
    end if

    if(present(SelectedOption))then
      VG%nVariables = size(SelectedOption)
    else if(present(DataOption))then
      VG%nVariables = size(DataOption, dim=2)
    else if(present(DataShapeOption))then
      VG%nVariables = DataShapeOption(2)
    end if
    
    if(present(VectorIndicesOption)) VG%nVectors = size(VectorIndicesOption)
    
    if(present(NameOption)) VG%lName = len_trim(NameOption)
    
    if(present(VariableOption))then
      allocate(VG%lVariable(size(VariableOption)))
      VG%lVariable = len_trim(VariableOption)
    end if

    if(present(VectorOption))then
      allocate(VG%lVector(size(VectorOption)))
      VG%lVector = len_trim(VectorOption)
    end if

    allocate(VG%Selected(VG%nVariables))
    if(present(SelectedOption))then
      VG%Selected = SelectedOption
    else
      VG%Selected = (/(iVariable, iVariable = 1, VG%nVariables)/)
    end if

    if(present(DataOption))then
      VG%Data => DataOption
      VG%LocalDataStorage = .false.
    else if(present(DataShapeOption))then
      allocate(VG%Data(VG%nData,DataShapeOption(2)))
      VG%LocalDataStorage = .true.
    else
      allocate(VG%Data(VG%nData,VG%nVariables))
      VG%LocalDataStorage = .true.
    end if
    
    if(present(NameOption)) VG%Name = trim(NameOption)
    
    if(present(VariableOption))then
      allocate(VG%Variable(size(VariableOption)))
      VG%Variable = VariableOption
    end if

    if(present(VectorOption))then
      allocate(VG%Vector(size(VectorOption)))
      VG%Vector = VectorOption
    end if

    allocate(VG%Unit(size(VG%Data, dim=2)))
    if(present(UnitOption)) VG%Unit = UnitOption
    
    if(present(VectorIndicesOption))then
      call Initialize( &
             VG%VectorIndices, VectorIndicesOption, CopyOption = .true.)
    else
      allocate(VG%VectorIndices(0))
    end if

  end subroutine Initialize_VG


  subroutine Initialize_VG_From_VG( &
               VG_Target, VG_Source, VectorIndicesOption, SelectedOption)

    type(VariableGroupBase), intent(inout) :: &
      VG_Target
    type(VariableGroupBase), intent(in) :: &
      VG_Source
    type(ArrayInteger_1D_Base), dimension(:), intent(in), optional ::&
      VectorIndicesOption
    integer(KI), dimension(:), intent(in), optional :: &
      SelectedOption
    
    call Finalize(VG_Target)
    
    VG_Target%nData = VG_Source%nData
    
    if(present(SelectedOption))then
      VG_Target%nVariables = size(SelectedOption)
    else
      VG_Target%nVariables = VG_Source%nVariables
    end if

    VG_Target%nVectors = VG_Source%nVectors
    VG_Target%lName    = VG_Source%lName

    if(allocated(VG_Source%lVariable))then
      allocate(VG_Target%lVariable(size(VG_Source%lVariable)))
      VG_Target%lVariable = VG_Source%lVariable
    end if

    if(allocated(VG_Source%lVector))then
      allocate(VG_Target%lVector(size(VG_Source%lVector)))
      VG_Target%lVector = VG_Source%lVector
    end if

    allocate(VG_Target%Selected(VG_Target%nVariables))
    if(present(SelectedOption))then
      VG_Target%Selected = SelectedOption
    else
      VG_Target%Selected = VG_Source%Selected
    end if
  
    VG_Target%Data => VG_Source%Data
    VG_Target%LocalDataStorage = .false.

    VG_Target%Name = VG_Source%Name

    if(allocated(VG_Source%Variable))then
      allocate(VG_Target%Variable(size(VG_Source%Variable)))
      VG_Target%Variable = VG_Source%Variable
    end if

    if(allocated(VG_Source%Vector))then
      allocate(VG_Target%Vector(size(VG_Source%Vector)))
      VG_Target%Vector = VG_Source%Vector
    end if

    if(allocated(VG_Source%Unit))then
      allocate(VG_Target%Unit(size(VG_Source%Unit)))
      VG_Target%Unit = VG_Source%Unit
    end if
  
    if(present(VectorIndicesOption))then
      call Initialize( &
             VG_Target%VectorIndices, VectorIndicesOption, &
             CopyOption = .true.)
    else
      call Initialize( &
             VG_Target%VectorIndices, VG_Source%VectorIndices, &
             CopyOption = .true.)
    end if
    
  end subroutine Initialize_VG_From_VG


  subroutine Finalize_VG(VG)

    type(VariableGroupBase), intent(inout) :: &
      VG

    call Finalize(VG%VectorIndices)

    if(allocated(VG%Unit))     deallocate(VG%Unit)
    if(allocated(VG%Vector))   deallocate(VG%Vector)
    if(allocated(VG%Variable)) deallocate(VG%Variable)

    VG%Name  = ''

    if(VG%LocalDataStorage)then
      deallocate(VG%Data)
    else
      VG%Data => null()
    end if

    if(allocated(VG%Selected))  deallocate(VG%Selected)
    if(allocated(VG%lVector))   deallocate(VG%lVector)
    if(allocated(VG%lVariable)) deallocate(VG%lVariable)

    VG%lName      = 0
    VG%nVectors   = 0
    VG%nVariables = 0
    VG%nData      = 0

  end subroutine Finalize_VG


end module VariableGroup_Base
