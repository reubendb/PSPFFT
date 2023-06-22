!-- This module defines an overloaded class which can be used as a building
!   block for "ragged arrays" of intrinsic data types. 

module Array_Base
  
  use Specifiers
  use Clear_Command
  use Copy_Command

  implicit none
  private

  public :: &
    Initialize, &
    Clear, &
    Finalize

  type, public :: ArrayInteger_1D_Base
    integer(KI), dimension(:), allocatable :: &
      Data
  end type ArrayInteger_1D_Base
  
  type, public :: ArrayInteger_1D_1D_Base
    type(ArrayInteger_1D_Base), dimension(:), allocatable :: &
      Data
  end type ArrayInteger_1D_1D_Base
  
  type, public :: ArrayBigInteger_1D_Base
    integer(KIB), dimension(:), allocatable :: &
      Data
  end type ArrayBigInteger_1D_Base

  type, public :: ArrayBigInteger_1D_1D_Base
    type(ArrayBigInteger_1D_Base), dimension(:), allocatable :: &
      Data
  end type ArrayBigInteger_1D_1D_Base

  type, public :: ArrayReal_1D_Base
    real(KR), dimension(:), allocatable :: &
      Data
  end type ArrayReal_1D_Base

  type, public :: ArrayReal_2D_Base
    real(KR), dimension(:,:), allocatable :: &
      Data
  end type ArrayReal_2D_Base
  
  type, public :: ArrayReal_3D_Base
    real(KR), dimension(:,:,:), allocatable :: &
      Data
  end type ArrayReal_3D_Base

  type, public :: ArrayReal_1D_2D_Base
    type(ArrayReal_2D_Base), dimension(:), allocatable :: &
      Data
  end type ArrayReal_1D_2D_Base
  
  type, public :: ArrayComplex_3D_Base
    complex(KC), dimension(:,:,:), allocatable :: &
      Data
  end type ArrayComplex_3D_Base

  type, public :: ArrayCharacter_1D_Base
    character(LL), dimension(:), allocatable :: &
      Data
  end type ArrayCharacter_1D_Base
  
  interface Initialize
    module procedure InitializeArrayInteger_1D
    module procedure InitializeArrayInteger_1D_1D
    module procedure InitializeArrayBigInteger_1D
    module procedure InitializeArrayBigInteger_1D_1D
    module procedure InitializeArrayReal_1D
    module procedure InitializeArrayReal_2D
    module procedure InitializeArrayReal_3D
    module procedure InitializeArrayReal_1D_2D
    module procedure InitializeArrayComplex_3D
    module procedure InitializeArrayCharacter_1D
    module procedure InitializeRaggedArrayInteger_1D
    module procedure InitializeRaggedArrayReal_2D
  end interface Initialize

  interface Clear
    module procedure ClearArrayInteger_1D_1D
  end interface Clear

  interface Finalize
    module procedure FinalizeArrayInteger_1D
    module procedure FinalizeArrayInteger_1D_1D
    module procedure FinalizeArrayBigInteger_1D
    module procedure FinalizeArrayBigInteger_1D_1D
    module procedure FinalizeArrayReal_1D
    module procedure FinalizeArrayReal_2D
    module procedure FinalizeArrayReal_3D
    module procedure FinalizeArrayReal_1D_2D
    module procedure FinalizeArrayComplex_3D
    module procedure FinalizeArrayCharacter_1D
    module procedure FinalizeRaggedArrayInteger_1D
    module procedure FinalizeRaggedArrayReal_2D
  end interface Finalize
  
contains


  subroutine InitializeArrayInteger_1D( &
               A, nData, ClearOption, LowerBoundOption)

    type(ArrayInteger_1D_Base), intent(inout) :: &
      A
    integer(KI), intent(in) :: &
      nData
    logical(KL), intent(in), optional :: &
      ClearOption
    integer(KI), intent(in), optional :: &
      LowerBoundOption

    integer(KI) :: &
      LowerBound
    logical(KL) :: &
      ClearRequested

    if(nData < 0) return
    
    ClearRequested = .false.
    if(present(ClearOption)) ClearRequested = ClearOption

    LowerBound = 1
    if(present(LowerBoundOption)) LowerBound = LowerBoundOption
    
    if(nData > 0)then
      allocate(A%Data(LowerBound:LowerBound+nData-1))
    else
      allocate(A%Data(0))
    end if

    if(ClearRequested) call Clear(A%Data)

  end subroutine InitializeArrayInteger_1D

  
  subroutine InitializeArrayInteger_1D_1D(A, nData, Shapes)

    type(ArrayInteger_1D_1D_Base), intent(inout) :: &
      A
    integer(KI), intent(in) :: &
      nData
    integer(KI), dimension(:), intent(in) :: &
      Shapes
    
    integer(KI) :: &
      iDatum

    if(nData < 0) return
    
    allocate(A%Data(nData))
    do iDatum = 1, nData
      call Initialize(A%Data(iDatum), Shapes(iDatum))      
    end do

  end subroutine InitializeArrayInteger_1D_1D


  subroutine InitializeArrayBigInteger_1D(A, nData, LowerBoundOption)

    type(ArrayBigInteger_1D_Base), intent(inout) :: &
      A
    integer(KI), intent(in) :: &
      nData
    integer(KI), intent(in), optional :: &
      LowerBoundOption

    integer(KI) :: &
      LowerBound

    if(nData < 0) return
    
    LowerBound = 1
    if(present(LowerBoundOption)) LowerBound = LowerBoundOption
    
    if(nData > 0)then
      allocate(A%Data(LowerBound:LowerBound+nData-1))
    else
      allocate(A%Data(0))
    end if

  end subroutine InitializeArrayBigInteger_1D


  subroutine InitializeArrayBigInteger_1D_1D(A, nData, Shapes)

    type(ArrayBigInteger_1D_1D_Base), intent(inout) :: &
      A
    integer(KI), intent(in) :: &
      nData
    integer(KI), dimension(:), intent(in) :: &
      Shapes
    
    integer(KI) :: &
      iDatum
    
    if(nData < 0) return
    
    allocate(A%Data(nData))
    do iDatum = 1, nData
      call Initialize(A%Data(iDatum), Shapes(iDatum))      
    end do

  end subroutine InitializeArrayBigInteger_1D_1D


  subroutine InitializeArrayReal_1D(A, nData, LowerBoundOption)

    type(ArrayReal_1D_Base), intent(inout) :: &
      A
    integer(KI), intent(in) :: &
      nData
    integer(KI), intent(in), optional :: &
      LowerBoundOption

    integer(KI) :: &
      LowerBound
    
    if(nData < 0) return

    LowerBound = 1
    if(present(LowerBoundOption)) LowerBound = LowerBoundOption
    
    if(nData > 0)then
      allocate(A%Data(LowerBound:LowerBound+nData-1))
    else
      allocate(A%Data(0))
    end if

  end subroutine InitializeArrayReal_1D


  subroutine InitializeArrayReal_2D(A, nData, ClearOption)

    type(ArrayReal_2D_Base), intent(inout) :: &
      A
    integer(KI), dimension(:), intent(in) :: &
      nData
    logical(KL), intent(in), optional :: &
      ClearOption

    logical(KL) :: &
      ClearRequested

    if(any(nData < 0)) return
    
    ClearRequested = .false.
    if(present(ClearOption)) ClearRequested = ClearOption
    
    allocate(A%Data(nData(1),nData(2)))
    
    if(ClearRequested) call Clear(A%Data)

  end subroutine InitializeArrayReal_2D

  
  subroutine InitializeArrayReal_3D(A, nData)

    type(ArrayReal_3D_Base), intent(inout) :: &
      A
    integer(KI), dimension(:), intent(in) :: &
      nData

    if(any(nData < 0)) return
    
    allocate(A%Data(nData(1),nData(2),nData(3)))

  end subroutine InitializeArrayReal_3D

  
  subroutine InitializeArrayReal_1D_2D(A, nData, Shapes)

    type(ArrayReal_1D_2D_Base), intent(inout) :: &
      A
    integer(KI), intent(in) :: &
      nData
    type(ArrayInteger_1D_Base), dimension(:), intent(in) :: &
      Shapes
    
    integer(KI) :: &
      iDatum
    
    if(nData < 0) return
    
    allocate(A%Data(nData))
    do iDatum = 1, nData
      call Initialize(A%Data(iDatum), Shapes(iDatum)%Data)
    end do

  end subroutine InitializeArrayReal_1D_2D
  
  
  subroutine InitializeArrayComplex_3D(A, nData)

    type(ArrayComplex_3D_Base), intent(inout) :: &
      A
    integer(KI), dimension(:), intent(in) :: &
      nData

    if(any(nData < 0)) return
    
    allocate(A%Data(nData(1),nData(2),nData(3)))

  end subroutine InitializeArrayComplex_3D


  subroutine InitializeArrayCharacter_1D(A, nData, LowerBoundOption)

    type(ArrayCharacter_1D_Base), intent(inout) :: &
      A
    integer(KI), intent(in) :: &
      nData
    integer(KI), intent(in), optional :: &
      LowerBoundOption

    integer(KI) :: &
      LowerBound

    if(nData < 0) return
    
    LowerBound = 1
    if(present(LowerBoundOption)) LowerBound = LowerBoundOption
    
    if(nData > 0)then
      allocate(A%Data(LowerBound:LowerBound+nData-1))
    else
      allocate(A%Data(0))
    end if

  end subroutine InitializeArrayCharacter_1D


  subroutine InitializeRaggedArrayInteger_1D(A, B, CopyOption)

    type(ArrayInteger_1D_Base), dimension(:), intent(inout), allocatable :: &
      A
    type(ArrayInteger_1D_Base), dimension(:), intent(in) :: &
      B
    logical(KL), intent(in), optional :: &
      CopyOption

    integer(KI) :: &
      iDatum
    logical(KL) :: &
      CopyRequested

    CopyRequested = .false.
    if(present(CopyOption)) CopyRequested = CopyOption

    allocate(A(size(B)))
    
    do iDatum = 1, size(A)
      call Initialize(A(iDatum), size(B(iDatum)%Data))
      if(CopyRequested) call Copy(B(iDatum)%Data, A(iDatum)%Data)
    end do

  end subroutine InitializeRaggedArrayInteger_1D

  
  subroutine InitializeRaggedArrayReal_2D(A, B)

    type(ArrayReal_2D_Base), dimension(:), intent(inout), allocatable :: &
      A
    type(ArrayReal_2D_Base), dimension(:), intent(in) :: &
      B

    integer(KI) :: &
      iDatum

    allocate(A(size(B)))
    
    do iDatum = 1, size(A)
      call Initialize(A(iDatum), shape(B(iDatum)%Data))
    end do

  end subroutine InitializeRaggedArrayReal_2D

  
  subroutine ClearArrayInteger_1D_1D(A)

    type(ArrayInteger_1D_1D_Base), intent(inout) :: &
      A
    
    integer(KI) :: &
      iDatum
    
    do iDatum = 1, size(A%Data)
      call Clear(A%Data(iDatum)%Data)
    end do

  end subroutine ClearArrayInteger_1D_1D
  

  subroutine FinalizeArrayInteger_1D(A)

    type(ArrayInteger_1D_Base), intent(inout) :: &
      A

    if(allocated(A%Data)) deallocate(A%Data)

  end subroutine FinalizeArrayInteger_1D
  
  
  subroutine FinalizeArrayInteger_1D_1D(A)

    type(ArrayInteger_1D_1D_Base), intent(inout) :: &
      A
    
    integer(KI) :: &
      iDatum
    
    if(allocated(A%Data))then
      do iDatum = 1, size(A%Data)
        call Finalize(A%Data(iDatum))
      end do
      deallocate(A%Data)
    end if

  end subroutine FinalizeArrayInteger_1D_1D
  
  
  subroutine FinalizeArrayBigInteger_1D(A)

    type(ArrayBigInteger_1D_Base), intent(inout) :: &
      A

    if(allocated(A%Data)) deallocate(A%Data)

  end subroutine FinalizeArrayBigInteger_1D

  
  subroutine FinalizeArrayBigInteger_1D_1D(A)

    type(ArrayBigInteger_1D_1D_Base), intent(inout) :: &
      A
    
    integer(KI) :: &
      iDatum

    if(allocated(A%Data))then
      do iDatum = 1, size(A%Data)
        call Finalize(A%Data(iDatum))
      end do
      deallocate(A%Data)
    end if

  end subroutine FinalizeArrayBigInteger_1D_1D
  
  
  subroutine FinalizeArrayReal_1D(A)

    type(ArrayReal_1D_Base), intent(inout) :: &
      A

    if(allocated(A%Data)) deallocate(A%Data)

  end subroutine FinalizeArrayReal_1D


  subroutine FinalizeArrayReal_2D(A)

    type(ArrayReal_2D_Base), intent(inout) :: &
      A

    if(allocated(A%Data)) deallocate(A%Data)

  end subroutine FinalizeArrayReal_2D
 

  subroutine FinalizeArrayReal_3D(A)

    type(ArrayReal_3D_Base), intent(inout) :: &
      A

    if(allocated(A%Data)) deallocate(A%Data)

  end subroutine FinalizeArrayReal_3D
 

  subroutine FinalizeArrayReal_1D_2D(A)

    type(ArrayReal_1D_2D_Base), intent(inout) :: &
      A
    
    integer(KI) :: &
      iDatum

    if(allocated(A%Data))then
      do iDatum = 1, size(A%Data)
        call Finalize(A%Data(iDatum))
      end do
      deallocate(A%Data)
    end if

  end subroutine FinalizeArrayReal_1D_2D
  
  
  subroutine FinalizeArrayComplex_3D(A)

    type(ArrayComplex_3D_Base), intent(inout) :: &
      A

    if(allocated(A%Data)) deallocate(A%Data)

  end subroutine FinalizeArrayComplex_3D
  
  
  subroutine FinalizeArrayCharacter_1D(A)

    type(ArrayCharacter_1D_Base), intent(inout) :: &
      A

    if(allocated(A%Data)) deallocate(A%Data)

  end subroutine FinalizeArrayCharacter_1D
  
  
  subroutine FinalizeRaggedArrayInteger_1D(A)

    type(ArrayInteger_1D_Base), dimension(:), intent(inout), allocatable :: &
      A

    integer(KI) :: &
      iDatum
    
    if(.not.allocated(A)) return
    
    do iDatum = 1, size(A)
      call Finalize(A(iDatum))
    end do

    deallocate(A)
    
  end subroutine FinalizeRaggedArrayInteger_1D

  
  subroutine FinalizeRaggedArrayReal_2D(A)

    type(ArrayReal_2D_Base), dimension(:), intent(inout), allocatable :: &
      A

    integer(KI) :: &
      iDatum

    do iDatum = 1, size(A)
      call Finalize(A(iDatum))
    end do

    if(allocated(A)) deallocate(A)
    
  end subroutine FinalizeRaggedArrayReal_2D

  
end module Array_Base
