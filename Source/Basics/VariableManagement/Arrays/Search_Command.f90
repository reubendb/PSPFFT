!-- This module provides abstraction to search a value in an array by 
!   overloading a generic "Search()" subroutine.

module Search_Command

  !-- based on Numerical Recipes, Fortran (1992), Fortran 90 (1996) 
  !   routine "locate"

  use Specifiers

  implicit none
  private

  public :: &
    Search

  interface Search
    module procedure SearchInteger
    module procedure SearchReal
    module procedure SearchString
  end interface Search

contains


  subroutine SearchInteger(A, Value, iValue)

    integer(KI), dimension(:), intent(in) :: &
      A
    integer(KI), intent(in) :: &
      Value
    integer(KI), intent(out) :: &
      iValue

    logical(KL) :: &
      Ascending
    integer(KI) :: &
      nElements, &
      iLow, &
      iMiddle, &
      iHigh
    
    nElements = size(A)
    Ascending = (A(nElements) >= A(1))
    iLow = 0
    iHigh = nElements + 1
    do while(iHigh - iLow > 1)
      iMiddle = (iHigh + iLow) / 2
      if(Ascending .eqv. (Value >= A(iMiddle)))then
        iLow = iMiddle
      else
        iHigh = iMiddle
      end if
    end do
    if(Value == A(1)) then
      iValue = 1
    else if(Value == A(nElements))then
!      iValue = nElements - 1
      iValue = nElements  !-- modification from Numerical Recipes for 
                         !   integer case
    else
      iValue = iLow
    end if
 
  end subroutine SearchInteger

  
  subroutine SearchReal(A, Value, iValue)

    real(KR), dimension(:), intent(in) :: &
      A
    real(KR), intent(in) :: &
      Value
    integer(KI), intent(out) :: &
      iValue

    logical(KL) :: &
      Ascending
    integer(KI) :: &
      nElements, &
      iLow, &
      iMiddle, &
      iHigh
    
    nElements = size(A)
    Ascending = (A(nElements) >= A(1))
    iLow = 0
    iHigh = nElements + 1
    do while(iHigh - iLow > 1)
      iMiddle = (iHigh + iLow) / 2
      if(Ascending .eqv. (Value >= A(iMiddle)))then
        iLow = iMiddle
      else
        iHigh = iMiddle
      end if
    end do
    if(Value == A(1)) then
      iValue = 1
    else if(Value == A(nElements))then
      iValue = nElements - 1
    else
      iValue = iLow
    end if
 
  end subroutine SearchReal
  
  
  subroutine SearchString(A, Value, iValue)
  
    !-- This uses linear search 
    character(*), dimension(:), intent(in) :: &
      A
    character(*), intent(in) :: &
      Value
    integer(KI), intent(out) :: &
      iValue
    
    logical(KL) :: &
      Found
   
    Found = .false. 
    do iValue = 1, size(A)
      if(trim(Value) == trim(A(iValue)))then
        Found = .true.
        exit
      end if
    end do
    
    if(.not.Found) iValue = 0
    
  end subroutine SearchString


end module Search_Command
