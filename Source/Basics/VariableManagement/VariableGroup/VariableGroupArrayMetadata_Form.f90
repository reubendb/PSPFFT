!-- This module defines a class that stores information about each element of 
!   an array of VariableGroups. 

module VariableGroupArrayMetadata_Form

  use Specifiers
  use VariableGroup_Base
  
  implicit none
  private

  public :: &
    Create, &
    Destroy

  type, public :: VariableGroupArrayMetadataForm
    integer(KI) :: &
      nGroups         = 0, &
      nVariablesTotal = 0
    integer(KI), dimension(:), allocatable :: &
      nVariables
    type(VariableGroupBase), dimension(:), pointer :: &
      VariableGroup => null()
  end type VariableGroupArrayMetadataForm

  interface Create
    module procedure Create_VGAM
  end interface Create

  interface Destroy
    module procedure Destroy_VGAM
  end interface Destroy

contains

  
  subroutine Create_VGAM(VGAM, VariableGroup)

    type(VariableGroupArrayMetadataForm), pointer :: &
      VGAM
    type(VariableGroupBase), dimension(:), intent(in), target :: &
      VariableGroup

    integer(KI) :: &
      iGroup

    allocate(VGAM)

    VGAM%nGroups = size(VariableGroup)

    allocate(VGAM%nVariables(VGAM%nGroups))
    VGAM%nVariables &
      = (/(size(VariableGroup(iGroup)%Selected), iGroup=1,VGAM%nGroups)/)

    VGAM%nVariablesTotal = sum(VGAM%nVariables)

    VGAM%VariableGroup => VariableGroup

  end subroutine Create_VGAM


  subroutine Destroy_VGAM(VGAM)

    type(VariableGroupArrayMetadataForm), pointer :: &
      VGAM

    VGAM%VariableGroup => null()

    VGAM%nVariablesTotal = 0

    deallocate(VGAM%nVariables)

    VGAM%nGroups = 0

    deallocate(VGAM)

  end subroutine Destroy_VGAM


end module VariableGroupArrayMetadata_Form
