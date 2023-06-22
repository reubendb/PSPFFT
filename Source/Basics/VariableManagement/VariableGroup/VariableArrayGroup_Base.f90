!-- This module defines a class that stores data for a group of ragged arrays,
!   together with some information about them (names, units, etc.). 

module VariableArrayGroup_Base

  use Specifiers
  use Arrays
  
  implicit none 
  private
  
  public :: &
    Initialize, &
    Finalize
  
  type, public :: VariableArrayReal_3D_GroupBase
    integer(KI) :: &
      nVariables = 0, &
      lName      = 0
    integer(KI), dimension(:), allocatable :: &
      lVariable, &
      Selected
    character(LL) :: &
      Name = ''
    character(LL), dimension(:), allocatable :: &
      Variable
    type(LabeledValueBase), dimension(:), allocatable :: &
      Unit
    type(ArrayInteger_1D_Base), dimension(:), allocatable :: &
      nData
    type(ArrayReal_3D_Base), dimension(:), pointer :: &
      Data => null()
  end type VariableArrayReal_3D_GroupBase

  interface Initialize
    module procedure Initialize_VAR_3D_G
    module procedure Initialize_VAR_3D_G_From_VAR
  end interface Initialize

  interface Finalize
    module procedure Finalize_VAR_3D_G
  end interface Finalize
  
contains

  
  subroutine Initialize_VAR_3D_G( &
               VAR, Data, UnitOption, VariableOption, NameOption, &
               SelectedOption)

    type(VariableArrayReal_3D_GroupBase), intent(inout) :: &
      VAR
    type(ArrayReal_3D_Base), dimension(:), intent(in), target :: &
      Data
    type(LabeledValueBase), dimension(:), intent(in), optional :: &
      UnitOption
    character(*), dimension(:), intent(in), target, optional :: &
      VariableOption
    character(*), intent(in), optional :: &
      NameOption
    integer(KI), dimension(:), intent(in), optional :: &
      SelectedOption

    integer(KI) :: &
      iVariable
      
    if(present(SelectedOption)) then
      VAR%nVariables = size(SelectedOption)
    else
      VAR%nVariables = size(Data)
    end if
    
    if(present(NameOption)) VAR%lName = len_trim(NameOption)
    
    if(present(VariableOption))then
      allocate(VAR%lVariable(size(VariableOption)))
      VAR%lVariable = len_trim(VariableOption)
    end if
    
    allocate(VAR%Selected(VAR%nVariables))
    if(present(SelectedOption))then
      VAR%Selected = SelectedOption
    else
      VAR%Selected = (/(iVariable, iVariable = 1, VAR%nVariables)/)
    end if
    
    if(present(NameOption)) VAR%Name = trim(NameOption)
    
    if(present(VariableOption))then
      allocate(VAR%Variable(size(VariableOption)))
      VAR%Variable = VariableOption
    end if
    
    allocate(VAR%Unit(size(Data)))
    if(present(UnitOption)) VAR%Unit = UnitOption
    
    allocate(VAR%nData(size(Data)))
    do iVariable = 1, size(Data)
      call Initialize(VAR%nData(iVariable), 3)
      VAR%nData(iVariable)%Data = -1
      if(allocated(Data(iVariable)%Data)) &
        VAR%nData(iVariable)%Data = shape(Data(iVariable)%Data)
    end do
    
    VAR%Data => Data    
    
  end subroutine Initialize_VAR_3D_G
  
  
  subroutine Initialize_VAR_3D_G_From_VAR( &
               VAR_Target, VAR_Source, SelectedOption)
               
    type(VariableArrayReal_3D_GroupBase), intent(inout) :: &
      VAR_Target
    type(VariableArrayReal_3D_GroupBase), intent(in) :: &
      VAR_Source
    integer(KI), dimension(:), intent(in), optional :: &
      SelectedOption
    
    integer(KI) :: &
      iVariable
      
    if(present(SelectedOption))then
      VAR_Target%nVariables = size(SelectedOption)
    else
      VAR_Target%nVariables = VAR_Source%nVariables
    end if
    
    VAR_Target%lName = VAR_Source%lName
    
    if(allocated(VAR_Source%lVariable))then
      allocate(VAR_Target%lVariable(size(VAR_Source%lVariable)))
      VAR_Target%lVariable = VAR_Source%lVariable
    end if
    
    allocate(VAR_Target%Selected(VAR_Target%nVariables))
    if(present(SelectedOption))then
      VAR_Target%Selected = SelectedOption
    else
      VAR_Target%Selected = VAR_Source%Selected
    end if
    
    VAR_Target%Name = VAR_Source%Name 
    
    if(allocated(VAR_Source%Variable))then
      allocate(VAR_Target%Variable(size(VAR_Source%Variable)))
      VAR_Target%Variable = VAR_Source%Variable
    end if
    
    if(allocated(VAR_Source%Unit))then
      allocate(VAR_Target%Unit(size(VAR_Source%Unit)))
      VAR_Target%Unit = VAR_Source%Unit   
    end if
    
    if(allocated(VAR_Source%nData))then
      allocate(VAR_Target%nData(size(VAR_Source%nData)))
      do iVariable = 1, size(VAR_Target%nData)
        call Initialize( &
               VAR_Target%nData(iVariable), &
               size(VAR_Source%nData(iVariable)%Data))
        VAR_Target%nData(iVariable)%Data = VAR_Source%nData(iVariable)%Data
      end do
    end if
    
    VAR_Target%Data => VAR_Source%Data    
  
  end subroutine Initialize_VAR_3D_G_From_VAR
  
  
  subroutine Finalize_VAR_3D_G(VAR)
  
    type(VariableArrayReal_3D_GroupBase), intent(inout) :: &
      VAR
      
    integer(KI) :: &
      iVariable
    
    VAR%Data => null()
    
    if(allocated(VAR%nData))then
      do iVariable = 1, size(VAR%nData)
        call Finalize(VAR%nData(iVariable))
      end do
      deallocate(VAR%nData)
    end if
      
    if(allocated(VAR%Unit)) deallocate(VAR%Unit)
    if(allocated(VAR%Variable)) deallocate(VAR%Variable)
    
    VAR%Name = ''
    
    if(allocated(VAR%Selected)) deallocate(VAR%Selected)
    if(allocated(VAR%lVariable)) deallocate(VAR%lVariable)
    
    VAR%lName      = 0
    VAR%nVariables = 0
    
  end subroutine Finalize_VAR_3D_G


end module VariableArrayGroup_Base
