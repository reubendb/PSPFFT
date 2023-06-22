!-- This module is a wrapper for MPI Barrier routine

module Block_Command
  
  use VariableManagement
  use MessagePassingBasics

  implicit none
  private

  include 'mpif.h'

  public :: &
    Block

  interface Block
    module procedure Block_C
  end interface Block

contains

  subroutine Block_C(C)
    
    type(CommunicatorForm), intent(in) :: &
      C
    
    integer(KI) :: &
      Error
      
    call MPI_BARRIER(C%Handle, Error)
      
  end subroutine Block_C

end module Block_Command
