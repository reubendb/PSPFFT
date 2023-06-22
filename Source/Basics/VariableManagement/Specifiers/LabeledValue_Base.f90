!-- LabeledValueBase is a class for a labeled value object, commonly used for
!   physical unit to store the unit string (label) and its conversion factor 
!   (value). 

module LabeledValue_Base
  
  use KIND_Singleton
  use LEN_Singleton
  
  implicit none
  private
  
  public :: &
    Initialize, &
    Value, &
    Finalize, &
    operator(+), &
    operator(-), &
    operator(*), &
    operator(/), &
    operator(**), &
    assignment(=)
  
  type, public :: LabeledValueBase
    real(KIND_REAL) :: &
      Value = 1.0_KIND_REAL
    character(LEN_LABEL) :: &
      Label = ''
  end type LabeledValueBase
  
  interface Initialize
    module procedure Initialize_LV
    module procedure Initialize_LV_From_LV
    module procedure Initialize_LV_From_LV_Label
  end interface Initialize
  
  interface Value
    module procedure Value_LV
  end interface Value

  interface Finalize
    module procedure Finalize_LV
  end interface Finalize
  
  interface operator(+)
    module procedure AdditionReal_LV
    module procedure Addition_LV_Real
    module procedure Addition_LV_LV
  end interface operator(+)
  
  interface operator(-)
    module procedure Subtraction_LV_Real
  end interface operator(-)
  
  interface operator(*)
    module procedure ProductReal_LV
    module procedure Product_LV_LV
    module procedure Product_LV_Real
  end interface operator(*)
  
  interface operator(/)
    module procedure QuotientReal_LV
    module procedure Quotient_LV_LV
    module procedure Quotient_LV_Real
  end interface operator(/)
  
  interface operator(**)
    module procedure Power_LV_Real
    module procedure Power_LV_Integer
  end interface operator(**)
  
  interface assignment(=)
    module procedure Assign_LV_Real
  end interface assignment(=)
  
contains


  subroutine Initialize_LV(LV, Label, Value)
    
    type(LabeledValueBase), intent(inout) :: &
      LV
    character(*), intent(in) :: &
      Label
    real(KIND_REAL), intent(in) :: &
      Value
      
    LV%Value = Value
    LV%Label = Label

  end subroutine Initialize_LV
  
  
  subroutine Initialize_LV_From_LV(LV_Target, LV_Source)
    
    type(LabeledValueBase), intent(inout) :: &
      LV_Target
    type(LabeledValueBase), intent(in) :: &
      LV_Source
            
    LV_Target%Value = LV_Source%Value
    LV_Target%Label = LV_Source%Label
  
  end subroutine Initialize_LV_From_LV
  
  
  subroutine Initialize_LV_From_LV_Label(LV_Target, Label, LV_Source)
    
    !-- Convention on argument ordering is broken here for the sake of
    !   consistency with the interface to Initialize_LV, in deference to
    !   the "rule of least surprise"
    !-- The argument LV_Source, while a derived type, 
    !   in this instance is being treated conceptually as a numerical value; 
    !   so it follows the spirit of the convention

    type(LabeledValueBase), intent(inout) :: &
      LV_Target
    character(*), intent(in) :: &
      Label
    type(LabeledValueBase), intent(in) :: &
      LV_Source
      
    LV_Target%Value = LV_Source%Value
    LV_Target%Label = Label
  
  end subroutine Initialize_LV_From_LV_Label
  
  
  elemental function Value_LV(LV) result(V)
   
    type(LabeledValueBase), intent(in) :: &
      LV
    real(KIND_REAL) :: &
      V
   
     V = LV%Value
   
  end function Value_LV
  

  subroutine Finalize_LV(LV)
    
    type(LabeledValueBase), intent(inout) :: &
      LV
      
    LV%Label = ''
    LV%Value = 1.0_KIND_REAL
  
  end subroutine Finalize_LV
  
  
  elemental function AdditionReal_LV(Augend, Addend) result(A_R_LV)
    
    !-- Convention on argument ordering is broken here to maintain 
    !   correspondence with order of entities in the expression
    !   "Augend + Addend" 
    
    real(KIND_REAL), intent(in) :: &
      Augend
    type(LabeledValueBase), intent(in) :: &
      Addend
    type(LabeledValueBase) :: &
      A_R_LV
    
    A_R_LV%Value = Augend + Addend%Value
    A_R_LV%Label = Addend%Label
  
  end function AdditionReal_LV
    
  
  elemental function Addition_LV_Real(Augend, Addend) result(A_LV_R)
    
    type(LabeledValueBase), intent(in) :: &
      Augend
    real(KIND_REAL), intent(in) :: &
      Addend
    type(LabeledValueBase) :: &
      A_LV_R
    
    A_LV_R%Value = Augend%Value + Addend
    A_LV_R%Label = Augend%Label
  
  end function Addition_LV_Real
    
  
  elemental function Addition_LV_LV(Augend, Addend) result(A_LV_LV)
  
    type(LabeledValueBase), intent(in) :: &
      Augend
    type(LabeledValueBase), intent(in) :: &
      Addend
    type(LabeledValueBase) :: &
      A_LV_LV
      
      A_LV_LV%Value = Augend%Value + Addend%Value
      
      if(trim(Augend%Label) == trim(Addend%Label))then
        A_LV_LV%Label = Augend%Label
      else
        A_LV_LV%Label = 'Undefined'
      end if
    
  end function Addition_LV_LV
  
  
  elemental function Subtraction_LV_Real(Minuend, Subtrahend) result(S_LV_R)
  
    type(LabeledValueBase), intent(in) :: &
      Minuend
    real(KIND_REAL), intent(in) :: &
      Subtrahend
    type(LabeledValueBase) :: &
      S_LV_R
    
    S_LV_R%Value = Minuend%Value - Subtrahend
    S_LV_R%Label = Minuend%Label
  
  end function Subtraction_LV_Real
  
  
  elemental function ProductReal_LV(Multiplier, Multiplicand) result(P_R_LV)
    
    !-- Convention on argument ordering is broken here to maintain 
    !   correspondence with order of entities in the expression
    !   "Multiplier * Multiplicand" 

    real(KIND_REAL), intent(in) :: &
      Multiplier
    type(LabeledValueBase), intent(in) :: &
      Multiplicand
    type(LabeledValueBase) :: &
      P_R_LV
    
    P_R_LV%Value = Multiplier * Multiplicand%Value
    P_R_LV%Label = Multiplicand%Label
    
  end function ProductReal_LV
  
  
  elemental function Product_LV_LV(Multiplier, Multiplicand) result(P_LV_LV)
    
    type(LabeledValueBase), intent(in) :: &
      Multiplier, &
      Multiplicand
    type(LabeledValueBase) :: &
      P_LV_LV
    
    P_LV_LV%Value = Multiplier%Value * Multiplicand%Value
    P_LV_LV%Label = trim(Multiplier%Label) // ' ' // trim(Multiplicand%Label)
    
  end function Product_LV_LV
  
  
  elemental function Product_LV_Real(Multiplier, Multiplicand) result(P_LV_R)
    
    type(LabeledValueBase), intent(in) :: &
      Multiplier
    real(KIND_REAL), intent(in) :: &
      Multiplicand
    type(LabeledValueBase) :: &
      P_LV_R
    
    P_LV_R%Value = Multiplier%Value * Multiplicand
    P_LV_R%Label = Multiplier%Label
    
  end function Product_LV_Real
  

  elemental function QuotientReal_LV(Dividend, Divisor) result(Q_R_LV)
    
    !-- Convention on argument ordering is broken here to maintain 
    !   correspondence with order of entities in the expression
    !   "Dividend / Divisor" 

    real(KIND_REAL), intent(in) :: &
      Dividend
    type(LabeledValueBase), intent(in) :: &
      Divisor
    type(LabeledValueBase) :: &
      Q_R_LV
 
    Q_R_LV = Dividend * Divisor**(-1)

  end function QuotientReal_LV
  
  
  elemental function Quotient_LV_LV(Dividend, Divisor) result(Q_LV_LV)
    
    type(LabeledValueBase), intent(in) :: &
      Dividend, &
      Divisor
    type(LabeledValueBase) :: &
      Q_LV_LV
    
    Q_LV_LV = Dividend * Divisor**(-1)

  end function Quotient_LV_LV
  
  
  elemental function Quotient_LV_Real(Dividend, Divisor) result(Q_LV_R)
    
    type(LabeledValueBase), intent(in) :: &
      Dividend
    real(KIND_REAL), intent(in) :: &
      Divisor
    type(LabeledValueBase) :: &
      Q_LV_R
    
    Q_LV_R = Dividend * Divisor**(-1)

  end function Quotient_LV_Real
  
  
  elemental function Power_LV_Real(Base, Exponent) result(P_LV_R)
    
    type(LabeledValueBase), intent(in) :: &
      Base
    real(KIND_REAL), intent(in) :: &
      Exponent
    type(LabeledValueBase) :: &
      P_LV_R
    
    integer(KIND_INTEGER) :: &
      Caret, &
      Dot, &
      OldIntegerExponent
    real(KIND_REAL) :: &
      OldRealExponent
    character(LEN_LABEL) :: &
      Scratch
    
    P_LV_R%Value = Base%Value ** Exponent
    
    Caret = index(Base%Label, '^')
    if(Caret > 0)then
      Dot = index(Base%Label(Caret+1:),'.')
      if(Dot > 0)then
        read(Base%Label(Caret+1:), fmt='(f7.4)') OldRealExponent
        write(Scratch, fmt='(f7.4)') (OldRealExponent * Exponent)
      else
        read(Base%Label(Caret+1:), fmt='(i7)') OldIntegerExponent
        write(Scratch, fmt='(f7.4)') (OldIntegerExponent *  Exponent)
      end if
      P_LV_R%Label = trim(Base%Label(:Caret)) // trim(adjustl(Scratch))
    else
      write(Scratch, fmt='(f7.4)') Exponent
      P_LV_R%Label = trim(Base%Label) // '^' // trim(adjustl(Scratch))
    end if
    
  end function Power_LV_Real
  

  elemental function Power_LV_Integer(Base, Exponent) result(P_LV_I)
    
    type(LabeledValueBase), intent(in) :: &
      Base
    Integer(KIND_INTEGER), intent(in) :: &
      Exponent
    type(LabeledValueBase) :: &
      P_LV_I
    
    integer(KIND_INTEGER) :: &
      Caret, &
      Dot, &
      OldIntegerExponent
    real(KIND_REAL) :: &
      OldRealExponent
    character(LEN_LABEL) :: &
      Scratch
      
    P_LV_I%Value = Base%Value ** Exponent
    
    Caret = index(Base%Label, '^')
    if(Caret > 0)then
      Dot = index(Base%Label(Caret+1:),'.')
      if(Dot > 0)then
        read(Base%Label(Caret+1:), fmt='(f7.4)') OldRealExponent
        write(Scratch, fmt='(f7.4)') (OldRealExponent * Exponent)
      else
        read(Base%Label(Caret+1:), fmt='(i7)') OldIntegerExponent
        write(Scratch, fmt='(i7)') (OldIntegerExponent *  Exponent)
      end if
      P_LV_I%Label = trim(Base%Label(:Caret)) // trim(adjustl(Scratch))
    else
      write(Scratch, fmt='(i7)') Exponent
      P_LV_I%Label = trim(Base%Label) // '^' // trim(adjustl(Scratch))
    end if
    
  end function Power_LV_Integer
  
  
  elemental subroutine Assign_LV_Real(LeftHandSide, RightHandSide)
    
    real(KIND_REAL), intent(inout) :: &
      LeftHandSide
    type(LabeledValueBase), intent(in) :: &
      RightHandSide
    
    LeftHandSide = RightHandSide%Value
  
  end subroutine Assign_LV_Real
  
    
end module LabeledValue_Base
