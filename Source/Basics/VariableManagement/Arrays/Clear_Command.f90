!-- This module provides abstraction to clear the content of an array 
!   (setting the value to zero) by overloading a generic "Clear()" subroutine.

module Clear_Command

  use Specifiers

  implicit none
  private

  public :: &
    Clear

  interface Clear
    module procedure ClearIntegerArray_1D
    module procedure ClearIntegerArray_2D
    module procedure ClearRealArray_1D
    module procedure ClearRealArray_2D
    module procedure ClearComplexArray_3D
    module procedure ClearLogicalArray_1D
  end interface Clear

contains


  subroutine ClearIntegerArray_1D(A)

    integer(KI), dimension(:), intent(out) :: &
      A
    
    A = 0_KI

  end subroutine ClearIntegerArray_1D

  
  subroutine ClearIntegerArray_2D(A)

    integer(KI), dimension(:,:), intent(out) :: &
      A

    A = 0_KI

  end subroutine ClearIntegerArray_2D


  subroutine ClearRealArray_1D(A)

    real(KR), dimension(:), intent(out) :: &
      A

    A = 0.0_KR

  end subroutine ClearRealArray_1D


  subroutine ClearRealArray_2D(A)

    real(KR), dimension(:,:), intent(out) :: &
      A

    A = 0.0_KR

  end subroutine ClearRealArray_2D
  
  
  subroutine ClearComplexArray_3D(A)

    complex(KC), dimension(:,:,:), intent(out) :: &
      A

    A = 0.0_KC

  end subroutine ClearComplexArray_3D
  

  subroutine ClearLogicalArray_1D(A)

    logical(KL), dimension(:), intent(out) :: &
      A

    A = .false.

  end subroutine ClearLogicalArray_1D
  
  
end module Clear_Command
