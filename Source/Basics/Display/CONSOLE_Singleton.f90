!-- This module defines a CONSOLE class, an object contains message verbosity
!   flags and labels, as well as the specification of the process rank that
!   displays to the "console" (i.e. stdout). This is used in conjuction with
!   the Show() subroutine (see Show_Command.f90).

module CONSOLE_Singleton

  use VariableManagement

  implicit none
  private

  public :: &
    Modify, &
    Mute, &
    Unmute

  integer(KI), public, parameter :: &
    CONSOLE_ERROR   = 1, &
    CONSOLE_WARNING = 2, &
    CONSOLE_INFO_1  = 3, &
    CONSOLE_INFO_2  = 4, &
    CONSOLE_INFO_3  = 5, &
    CONSOLE_INFO_4  = 6, &
    CONSOLE_INFO_5  = 7, &
    CONSOLE_INFO_6  = 8, &
    CONSOLE_INFO_7  = 9

    integer(KI), private, parameter :: &
      N_LABELS = 9

  character(LL), public, dimension(1:N_LABELS) :: &
    CONSOLE_LABEL &
      = (/'ERROR                         ', &
          'WARNING                       ', &
          'INFO_1                        ', &
          'INFO_2                        ', &
          'INFO_3                        ', &
          'INFO_4                        ', &
          'INFO_5                        ', &
          'INFO_6                        ', &
          'INFO_7                        '/)

  type, public :: ConsoleSingleton
    integer(KI) :: &
      Verbosity   = CONSOLE_INFO_1, &
      DisplayRank = 0, &
      ProcessRank = -1
    logical(KL) :: &
      Mute = .false.
  end type ConsoleSingleton

  type(ConsoleSingleton), public, save :: &
    CONSOLE

  interface Modify
    module procedure Modify_C
  end interface Modify
  
  interface Mute
    module procedure Mute_C
  end interface Mute
  
  interface Unmute
    module procedure Unmute_C
  end interface Unmute

contains

  
  subroutine Modify_C( &
               C, VerbosityOption, DisplayRankOption, &
               ProcessRankOption, ShowMessage, ShowCharacter, &
               ShowInteger)

    type(ConsoleSingleton), intent(inout) :: &
      C
    character(*), intent(in), optional :: &
      VerbosityOption
    integer(KI), intent(in), optional :: &
      DisplayRankOption, &
      ProcessRankOption
    include 'ShowMessage_Interface'
    include 'ShowCharacter_Interface'
    include 'ShowInteger_Interface'

    call ShowMessage('Modifying CONSOLE', CONSOLE_INFO_1, ProcessRankOption)

    if(present(VerbosityOption))then
      select case (trim(VerbosityOption))
        case ('ERROR')
          C%Verbosity = CONSOLE_ERROR
        case ('WARNING')
          C%Verbosity = CONSOLE_WARNING
        case ('INFO_1')
          C%Verbosity = CONSOLE_INFO_1
        case ('INFO_2')
          C%Verbosity = CONSOLE_INFO_2
        case ('INFO_3')
          C%Verbosity = CONSOLE_INFO_3
        case ('INFO_4')
          C%Verbosity = CONSOLE_INFO_4
        case ('INFO_5')
          C%Verbosity = CONSOLE_INFO_5
        case ('INFO_6')
          C%Verbosity = CONSOLE_INFO_6
        case ('INFO_7')
          C%Verbosity = CONSOLE_INFO_7
        case default
          call ShowMessage( &
                 'Unknown display verbosity. Reverting to default.', &
                 CONSOLE_WARNING, ProcessRankOption)
      end select
      call ShowCharacter( &
             CONSOLE_LABEL(C%Verbosity), 'Verbosity', CONSOLE_INFO_1, &
             ProcessRankOption)
    end if

    if(present(DisplayRankOption))then
      C%DisplayRank = DisplayRankOption
      call ShowInteger( &
             C%DisplayRank, 'DisplayRank', CONSOLE_INFO_1, ProcessRankOption)
    end if

    if(present(ProcessRankOption))then
      C%ProcessRank = ProcessRankOption
      call ShowInteger( &
             C%ProcessRank, 'ProcessRank', CONSOLE_INFO_1, ProcessRankOption)
    end if

  end subroutine Modify_C
  
  
  subroutine Mute_C(C)
    
    type(ConsoleSingleton), intent(inout) :: &
      C
      
      C%Mute = .true.
  
  end subroutine Mute_C
  
  
  subroutine Unmute_C(C)
    
    type(ConsoleSingleton), intent(inout) :: &
      C
      
      C%Mute = .false.
  
  end subroutine Unmute_C


end module CONSOLE_Singleton
