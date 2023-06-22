!-- This module provides abstraction to set the content of an array 
!   by overloading a generic "Set()" subroutine.

module Set_Command

  use Specifiers

  implicit none
  private

  public :: &
    Set

  interface Set
    module procedure SetIntegerArray_1D
  end interface Set

contains


  subroutine SetIntegerArray_1D(A, Value)

    integer(KI), dimension(:), intent(inout) :: &
      A
    integer(KI), intent(in) :: &
      Value
    
    A = Value

  end subroutine SetIntegerArray_1D


end module Set_Command
