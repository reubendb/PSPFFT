!-- This module provides abstraction to copy an array to another by 
!   overloading a generic "Copy()" subroutine.

module Copy_Command

  use Specifiers

  implicit none
  private

  public :: &
    Copy

  interface Copy
    module procedure CopyInteger_1D
    module procedure CopyInteger_1D_Section
    module procedure CopyInteger_2D
    module procedure CopyInteger_2D_Section
    module procedure CopyReal_1D
    module procedure CopyReal_1D_Section
    module procedure CopyReal_2D
    module procedure CopyReal_2D_Section
    module procedure CopyComplex_1D
    module procedure CopyComplex_1D_Section
    module procedure CopyComplex_3D
    module procedure CopyComplex_3D_Section
  end interface Copy

contains


  subroutine CopyInteger_1D(A, B)

    integer(KI), dimension(:), intent(in) :: &
      A
    integer(KI), dimension(:), intent(out) :: &
      B

    B = A

  end subroutine CopyInteger_1D
  
  
  subroutine CopyInteger_1D_Section(A, B, SourceOffset, TargetOffset, nData)
    
    integer(KI), dimension(:), intent(inout) :: &
      A
    integer(KI), dimension(:), intent(inout) :: &
      B
    integer(KI), intent(in) :: &
      SourceOffset, &
      TargetOffset, &
      nData

    call Copy( &
           A(SourceOffset+1:SourceOffset+nData), &
           B(TargetOffset+1:TargetOffset+nData))

  end subroutine CopyInteger_1D_Section
  

  subroutine CopyInteger_2D(A, B)

    integer(KI), dimension(:,:), intent(in) :: &
      A
    integer(KI), dimension(:,:), intent(out) :: &
      B

    B = A

  end subroutine CopyInteger_2D
  
  
  subroutine CopyInteger_2D_Section(A, B, SourceOffset, TargetOffset, nData)

    integer(KI), dimension(:,:), intent(inout) :: &
      A
    integer(KI), dimension(:,:), intent(inout) :: &
      B
    integer(KI), dimension(2), intent(in) :: &
      SourceOffset, &
      TargetOffset, &
      nData
      
    integer(KI) :: &
      iDatum_2

    do iDatum_2 = 1, nData(2)
      call Copy( &
             A(SourceOffset(1)+1:SourceOffset(1)+nData(1), &
               SourceOffset(2)+iDatum_2), &
             B(TargetOffset(1)+1:TargetOffset(1)+nData(1), &
               TargetOffset(2)+iDatum_2))
    end do

  end subroutine CopyInteger_2D_Section
  

  subroutine CopyReal_1D(A, B)

    real(KR), dimension(:), intent(in) :: &
      A
    real(KR), dimension(:), intent(out) :: &
      B

    B = A

  end subroutine CopyReal_1D
  
  
  subroutine CopyReal_1D_Section(A, B, SourceOffset, TargetOffset, nData)
    
    real(KR), dimension(:), intent(inout) :: &
      A
    real(KR), dimension(:), intent(inout) :: &
      B
    integer(KI), intent(in) :: &
      SourceOffset, &
      TargetOffset, &
      nData

    call Copy( &
           A(SourceOffset+1:SourceOffset+nData), &
           B(TargetOffset+1:TargetOffset+nData))

  
  end subroutine CopyReal_1D_Section
  

  subroutine CopyReal_2D(A, B)

    real(KR), dimension(:,:), intent(in) :: &
      A
    real(KR), dimension(:,:), intent(out) :: &
      B

    B = A

  end subroutine CopyReal_2D


  subroutine CopyReal_2D_Section(A, B, SourceOffset, TargetOffset, nData)

    real(KR), dimension(:,:), intent(inout) :: &
      A
    real(KR), dimension(:,:), intent(inout) :: &
      B
    integer(KI), dimension(2), intent(in) :: &
      SourceOffset, &
      TargetOffset, &
      nData
      
    integer(KI) :: &
      iDatum_2

    do iDatum_2 = 1, nData(2)
      call Copy( &
             A(SourceOffset(1)+1:SourceOffset(1)+nData(1), &
               SourceOffset(2)+iDatum_2), &
             B(TargetOffset(1)+1:TargetOffset(1)+nData(1), &
               TargetOffset(2)+iDatum_2))
    end do

  end subroutine CopyReal_2D_Section
  
  
  subroutine CopyComplex_1D(A, B)

    complex(KC), dimension(:), intent(in) :: &
      A
    complex(KC), dimension(:), intent(out) :: &
      B

    B = A

  end subroutine CopyComplex_1D
  
  
  subroutine CopyComplex_1D_Section(A, B, SourceOffset, TargetOffset, nData)
    
    complex(KC), dimension(:), intent(inout) :: &
      A
    complex(KC), dimension(:), intent(inout) :: &
      B
    integer(KI), intent(in) :: &
      SourceOffset, &
      TargetOffset, &
      nData

    call Copy( &
           A(SourceOffset+1:SourceOffset+nData), &
           B(TargetOffset+1:TargetOffset+nData))

  end subroutine CopyComplex_1D_Section
  
  
  subroutine CopyComplex_3D(A, B)

    complex(KC), dimension(:,:,:), intent(in) :: &
      A
    complex(KC), dimension(:,:,:), intent(out) :: &
      B

    B = A

  end subroutine CopyComplex_3D
  
  
  subroutine CopyComplex_3D_Section(A, B, SourceOffset, TargetOffset, nData)

    complex(KC), dimension(:,:,:), intent(inout) :: &
      A
    complex(KC), dimension(:,:,:), intent(inout) :: &
      B
    integer(KI), dimension(3), intent(in) :: &
      SourceOffset, &
      TargetOffset, &
      nData
      
    integer(KI) :: &
      iDatum_2, &
      iDatum_3
      
    do iDatum_3 = 1, nData(3)
      do iDatum_2 = 1, nData(2)
        call Copy( &
               A(SourceOffset(1)+1:SourceOffset(1)+nData(1), &
                 SourceOffset(2)+iDatum_2, SourceOffset(3)+iDatum_3), &
               B(TargetOffset(1)+1:TargetOffset(1)+nData(1), &
                 TargetOffset(2)+iDatum_2, TargetOffset(3)+iDatum_3))
      end do
    end do

  end subroutine CopyComplex_3D_Section
  
  
end module Copy_Command
