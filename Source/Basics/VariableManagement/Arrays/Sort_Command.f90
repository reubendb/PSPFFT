!-- This module provides abstraction to sort an array by overloading 
!   a generic "Sort()" subroutine.

module Sort_Command

  !-- Quick sort, based on http://www.fortran.com/qsort_c.f95
  !   by Juli Rew, SCD Consulting (juliana@ucar.edu), 9/03, 
  !   based in turn on algorithm from Cormen et al., 
  !   Introduction to Algorithms, 1997 printing

  use Specifiers

  implicit none
  private

  public :: &
    Sort

  interface Sort
    module procedure SortInteger
  end interface Sort

    private :: &
      FindPartitionIndex

    interface FindPartitionIndex
      module procedure FindPartitionIndexInteger
    end interface FindPartitionIndex

contains


  recursive subroutine SortInteger(A)

    integer(KI), dimension(:), intent(inout) :: &
      A

    integer(KI) :: &
      iPartition

    if(size(A) > 1)then
      call FindPartitionIndex(A, iPartition)
      call Sort(A(:iPartition-1))
      call Sort(A(iPartition:))
    end if

  end subroutine SortInteger


  subroutine FindPartitionIndexInteger(A, iPartition)

    integer(KI), dimension(:), intent(inout) :: &
      A
    integer(KI), intent(out) :: &
      iPartition

    integer(KI) :: &
      iLow, iHigh, &
      Swap, &
      Pivot

    Pivot = A(1)
    iLow  = 0
    iHigh = size(A) + 1

    do

      iHigh = iHigh - 1
      do while(A(iHigh) > Pivot)
        iHigh = iHigh - 1
      end do

      iLow = iLow + 1
      do while(A(iLow) < Pivot)
        iLow = iLow + 1
      end do

      if(iLow < iHigh)then
        Swap     = A(iLow)
        A(iLow)  = A(iHigh)
        A(iHigh) = Swap
      else if(iLow == iHigh)then
        iPartition = iLow + 1
        return
      else
        iPartition = iLow
        return
      end if

    end do

  end subroutine FindPartitionIndexInteger


end module Sort_Command
