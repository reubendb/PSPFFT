!-- VoidBase provides abstracted data type to be used with transfer() 
!-- instrinsic function.

module Void_Base

  use KIND_Singleton
  
  implicit none
  private
  
  public :: &
    Initialize, &
    Finalize
    
  type, public :: VoidBase
    character(1), allocatable, dimension(:) :: &
      Data
  end type VoidBase
  
  interface Initialize
    module procedure Initialize_V
  end interface Initialize
  
  interface Finalize
    module procedure Finalize_V
  end interface Finalize  
  
contains


  subroutine Initialize_V(V, Size)
  
    type(VoidBase), intent(inout) :: &
      V
    integer(KIND_INTEGER), intent(in) :: &
      Size
      
    allocate(V%Data(Size))
  
  end subroutine Initialize_V
  
  
  subroutine Finalize_V(V)
    
    type(VoidBase), intent(inout) :: &
      V
      
    deallocate(V%Data)
    
  end subroutine Finalize_V
  

end module Void_Base
