!--  This module defines a class based on MPI communicators. The routine to 
!    create a communicator object is overloaded to provide a generic Create()
!    interface that can also create MPI sub-communicator. 

module Communicator_Form
  
  use VariableManagement
  use Display

  implicit none
  private

  include 'mpif.h'

  public :: &
    Create, &
    Abort, &
    Destroy

  type, public :: CommunicatorForm
    integer(KI) :: &
      Handle, &
      Size, &
      Rank
    integer(KI), dimension(:), allocatable :: &
      RankIndex
    character(LL) :: &
      Name = ''
    type(CommunicatorForm), pointer :: &
      Parent => null()
  end type CommunicatorForm
  
  interface Create
    module procedure Create_C
    module procedure Create_C_FromHandle
  end interface Create

  interface Abort
    module procedure Abort_C
  end interface Abort
  
  interface Destroy
    module procedure Destroy_C
  end interface Destroy

contains

  
  subroutine Create_C(C, C_Option, WorldOption, NameOption, RanksOption)

    type(CommunicatorForm), pointer :: &
      C
    type(CommunicatorForm), intent(in), target, optional :: &
      C_Option
    logical(KL), intent(in), optional :: &
      WorldOption
    character(*), intent(in), optional :: &
      NameOption
    integer(KI), dimension(:), intent(in), optional :: &
      RanksOption

    logical(KL) :: &
      World
    integer(KI) :: &
      iRank, &
      Error, &
      OldGroup, NewGroup
    
    World = .false.
    if(present(WorldOption)) World = WorldOption

    if(present(C_Option) .and. .not.present(RanksOption))then

      !-- Point to an existing communicator

      C => C_Option

    else

      allocate(C)

      if(World)then

        !-- Initialize MPI and the "World" communicator

        call MPI_INIT(Error)
        C%Handle = MPI_COMM_WORLD
        call MPI_COMM_SIZE(C%Handle, C%Size, Error)
        call MPI_COMM_RANK(C%Handle, C%Rank, Error)

        allocate(C%RankIndex(0:C%Size-1))
        C%RankIndex = (/(iRank, iRank = 0, C%Size-1)/)

        C%Name = 'World'

        call Show( &
               'Creating a Communicator', CONSOLE_INFO_1, &
               ProcessRankOption = C%Rank)
        call Show( &
               C%Name, 'Name', CONSOLE_INFO_1, ProcessRankOption = C%Rank)
        call Show( &
               C%Size, 'Size', CONSOLE_INFO_1, ProcessRankOption = C%Rank)

      else if(present(C_Option) .and. present(RanksOption))then

        !-- Create subcommunicator

        call MPI_COMM_GROUP(C_Option%Handle, OldGroup, Error)
        call MPI_GROUP_INCL( &
               OldGroup, size(RanksOption), RanksOption, NewGroup, Error)
        call MPI_COMM_CREATE(C_Option%Handle, NewGroup, C%Handle, Error)
        call MPI_GROUP_FREE(NewGroup, Error)
        call MPI_GROUP_FREE(OldGroup, Error)
        call MPI_COMM_SIZE(C%Handle, C%Size, Error)
        call MPI_COMM_RANK(C%Handle, C%Rank, Error)

        allocate(C%RankIndex(0:C%Size-1))
        C%RankIndex = (/(iRank, iRank = 0, C%Size-1)/)

        if(present(NameOption)) C%Name = NameOption

        C%Parent => C_Option

        call Show('Creating a Communicator', CONSOLE_INFO_2)
        call Show(C%Parent%Name, 'Parent', CONSOLE_INFO_2)
        call Show(C%Name, 'Name', CONSOLE_INFO_2)
        call Show(C%Size, 'Size', CONSOLE_INFO_2)

      else

        call Show( &
               'More information needed to create a communicator', &
               CONSOLE_ERROR)
        stop

      end if

    end if
    
  end subroutine Create_C


  subroutine Create_C_FromHandle(C, Handle, NameOption)

    type(CommunicatorForm), pointer :: &
      C
    integer(KI), intent(in)  :: &
      Handle
    character(*), intent(in), optional :: &
      NameOption
    
    integer(KI) :: &
      iRank, &
      Error
    
    allocate(C)
    C%Handle = Handle
    call MPI_COMM_SIZE(C%Handle, C%Size, Error)
    call MPI_COMM_RANK(C%Handle, C%Rank, Error)
    
    allocate(C%RankIndex(0:C%Size-1))
    C%RankIndex = (/(iRank, iRank = 0, C%Size-1)/)
    
    if(present(NameOption))then
      C%Name = NameOption
    else
      C%Name = 'World'
    end if
    
    call Show('Creating a Communicator', CONSOLE_INFO_2)
    call Show(C%Name, 'Name', CONSOLE_INFO_2)
    call Show(C%Size, 'Size', CONSOLE_INFO_2)

  end subroutine Create_C_FromHandle


  subroutine Abort_C(C)

    type(CommunicatorForm), intent(in) :: &
      C

    integer(KI) :: &
      Error

    call MPI_ABORT(C%Handle, 0, Error)

  end subroutine Abort_C


  subroutine Destroy_C(C, C_Option, WorldOption, DestroyHandleOption)

    type(CommunicatorForm), pointer :: &
      C
    type(CommunicatorForm), intent(in), target, optional :: &
      C_Option
    logical(KL), intent(in), optional :: &
      WorldOption, &
      DestroyHandleOption

    logical(KL) :: &
      World, &
      DestroyHandle
    integer(KI) :: &
      Error

    World = .false.
    if(present(WorldOption)) World = WorldOption

    DestroyHandle = .true.
    if(present(DestroyHandleOption)) DestroyHandle = DestroyHandleOption

    if(present(C_Option))then
      C => null()
    else 
      if(World)then
        call Show('Destroying a Communicator', CONSOLE_INFO_1)
        call Show(C%Name, 'Name', CONSOLE_INFO_1)
        call MPI_FINALIZE(Error)
      else
        call Show('Destroying a Communicator', CONSOLE_INFO_2)
        call Show(C%Name, 'Name', CONSOLE_INFO_2)
        if(DestroyHandle) call MPI_COMM_FREE(C%Handle, Error)
      end if
      deallocate(C%RankIndex)
      deallocate(C)
    end if
        
  end subroutine Destroy_C


end module Communicator_Form
