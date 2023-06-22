!-- This module defines a class with some basic information about an
!   infrastructure for message passing.

module PortalHeader_Form

  use VariableManagement

  implicit none
  private

  public :: &
    Create, &
    Destroy

  type, public :: PortalHeaderForm
    integer(KI) :: &
      nSources = 0, &
      nTargets = 0
    integer(KI), dimension(:), allocatable :: &
      nChunksFrom, &
      nChunksTo, &
      Source, &
      Target
  end type PortalHeaderForm
  
  interface Create
    module procedure Create_PM
  end interface Create
  
  interface Destroy
    module procedure Destroy_PM
  end interface Destroy

contains

 
  subroutine Create_PM(PM, Source, Target, nChunksFrom, nChunksTo)

    type(PortalHeaderForm), pointer :: &
      PM
    integer(KI), dimension(:), intent(in) :: &
      Source, &
      Target, &
      nChunksFrom, &
      nChunksTo
    
    allocate(PM)
    
    allocate(PM%nChunksFrom(size(nChunksFrom)))
    PM%nChunksFrom = nChunksFrom
    
    allocate(PM%nChunksTo(size(nChunksTo)))
    PM%nChunksTo = nChunksTo
    
    PM%nSources = size(Source)
    allocate(PM%Source(PM%nSources))
    PM%Source = Source
      
    PM%nTargets = size(Target)
    allocate(PM%Target(PM%nTargets))
    PM%Target = Target

  end subroutine Create_PM


  subroutine Destroy_PM(PM)

    type(PortalHeaderForm), pointer :: &
      PM
    
    if(.not.associated(PM)) return
    
    if(allocated(PM%Target)) deallocate(PM%Target)
    if(allocated(PM%Source)) deallocate(PM%Source)
    if(allocated(PM%nChunksTo)) deallocate(PM%nChunksTo)
    if(allocated(PM%nChunksFrom)) deallocate(PM%nChunksFrom)
    
    deallocate(PM)

  end subroutine Destroy_PM


end module PortalHeader_Form
