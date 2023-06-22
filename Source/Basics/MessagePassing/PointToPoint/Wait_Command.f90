!-- This module defines wrappers for the MPI Wait routines, 
!   providing a generic Wait() subroutine by overloading it for different 
!   data types.

module Wait_Command

  use VariableManagement

  implicit none
  private
  
  include 'mpif.h'
  
  public :: &
    Wait
  
  interface Wait
    module procedure WaitSingle
    module procedure WaitAll_1D
    module procedure WaitAny_1D
  end interface Wait
   
contains

  
  subroutine WaitSingle(Handle)
    
    integer(KI), intent(inout) :: &
      Handle
    
    integer(KI) :: &
      Error
      
    call MPI_WAIT(Handle, MPI_STATUS_IGNORE, Error)
    
  end subroutine WaitSingle


  subroutine WaitAll_1D(Handle)
    
    integer(KI), dimension(:), intent(inout) :: &
      Handle
    
    integer(KI) :: &
      Error
      
    if(size(Handle) <= 0) return
    
    call MPI_WAITALL(size(Handle), Handle, MPI_STATUSES_IGNORE, Error)
    
  end subroutine WaitAll_1D
  
  
  subroutine WaitAny_1D(Handle, AllFinished, Index)
    
    integer(KI), dimension(:), intent(inout) :: &
      Handle
    logical(KL), intent(out) :: &
      AllFinished
    integer(KI), intent(out) :: &
      Index
    
    integer(KI) :: &
      Error
      
    if(size(Handle) <= 0)then
      AllFinished = .true.
      return
    end if
    
    call MPI_WAITANY(size(Handle), Handle, Index, MPI_STATUSES_IGNORE, Error)
    
    AllFinished = .false.
    if(Index == MPI_UNDEFINED) &
      AllFinished = .true.
    
  end subroutine WaitAny_1D
  

end module Wait_Command
