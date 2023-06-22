!-- This module defines an overloaded routine that shows either a message or a
!   piece of data (one of the intrinsic types) together with a label. 

module Show_Command
  
  use VariableManagement
  use CONSOLE_Singleton

  implicit none
  private

  public :: &
    Show, &
    ShowInteger, &   !-- These three routines are public because they are
    ShowCharacter, & !   used as subroutine arguments in conjunction with
    ShowMessage      !   an interface.

  interface Show
    module procedure ShowInteger
    module procedure ShowIntegerArray
    module procedure ShowReal
    module procedure ShowRealUnitized
    module procedure ShowRealArray
    module procedure ShowLogical
    module procedure ShowCharacter
    module procedure ShowMessage
  end interface Show

    private :: &
      PrepareShow

contains


  subroutine ShowInteger( &
               Number, Description, IgnorabilityOption, &
               ProcessRankOption, nLeadingBlankLinesOption)

    !-- Convention on argument order violated because the Number being
    !   "Show"n is more important than the Description.
    !-- Must agree with Basics/Display/ShowInteger_Interface.

    integer(KI), intent(in) :: &
      Number
    character(*), intent(in) :: &
      Description
    integer(KI), intent(in), optional :: &
      IgnorabilityOption, &
      ProcessRankOption, &
      nLeadingBlankLinesOption
      
    logical(KL) :: &
      AbortShow
    
    call PrepareShow( &
           AbortShow, IgnorabilityOption, ProcessRankOption, &
           nLeadingBlankLinesOption)

    if(AbortShow) return

    print '(a35,a3,i10)', trim(Description), '  =', Number
    
  end subroutine ShowInteger  
  
  
  subroutine ShowIntegerArray( &
               Array, iArray, IgnorabilityOption, ProcessRankOption, &
               nLeadingBlankLinesOption)
    
    integer(KI), dimension(:), intent(in) :: &
      Array
    integer(KI), intent(in) :: &
      iArray
    integer(KI), intent(in), optional :: &
      IgnorabilityOption, &
      ProcessRankOption, &
      nLeadingBlankLinesOption

    logical(KL) :: &
      AbortShow
    
    call PrepareShow( &
           AbortShow, IgnorabilityOption, ProcessRankOption, &
           nLeadingBlankLinesOption)

    if(AbortShow) return

    print *, '           ', iArray, ':', Array

  end subroutine ShowIntegerArray
  
  
  subroutine ShowReal( &
               Number, Description, IgnorabilityOption, &
               ProcessRankOption, nLeadingBlankLinesOption)

    !-- Convention on argument order violated because the Number being
    !   "Show"n is more important than the Description.

    real(KR), intent(in) :: &
      Number
    character(*), intent(in), optional :: &
      Description
    integer(KI), intent(in), optional :: &
      IgnorabilityOption, &
      ProcessRankOption, &
      nLeadingBlankLinesOption

    logical(KL) :: &
      AbortShow

    call PrepareShow( &
           AbortShow, IgnorabilityOption, ProcessRankOption, &
           nLeadingBlankLinesOption)

    if(AbortShow) return

    print '(a35,a3,es15.6e3)', trim(Description), '  =', Number
    
  end subroutine ShowReal
  
  
  subroutine ShowRealUnitized( &
               Number, Unit, Description, IgnorabilityOption, &
               ProcessRankOption, nLeadingBlankLinesOption)

    !-- Convention on argument order violated because the Number being
    !   "Show"n is more important than the Unit or the Description.

    real(KR), intent(in) :: &
      Number
    type(LabeledValueBase), intent(in) :: &
      Unit
    character(*), intent(in) :: &
      Description
    integer(KI), intent(in), optional :: &
      IgnorabilityOption, &
      ProcessRankOption, &
      nLeadingBlankLinesOption
    
    integer(KI) :: &
      LabelLen
    logical(KL) :: &
      AbortShow
    character(LL) :: &
      PrintFormat
          
    call PrepareShow( &
           AbortShow, IgnorabilityOption, ProcessRankOption, &
           nLeadingBlankLinesOption)

    if(AbortShow) return

    LabelLen = len_trim(Unit%Label) + 1
    if(LabelLen < 10)then
      write(PrintFormat, fmt='(a18,i1,a1)') '(a35,a3,es15.6e3,a',LabelLen,')'
    else
      write(PrintFormat, fmt='(a18,i2,a1)') '(a35,a3,es15.6e3,a',LabelLen,')'
    end if
      
    write(*, fmt=trim(PrintFormat)) &
      trim(Description), '  =', Number / Unit%Value, ' ' // trim(Unit%Label)
    
  end subroutine ShowRealUnitized
  
  
  subroutine ShowRealArray( &
               Array, iArray, IgnorabilityOption, &
               ProcessRankOption, nLeadingBlankLinesOption)
    
    real(KR), dimension(:), intent(in) :: &
      Array
    integer(KI), intent(in) :: &
      iArray
    integer(KI), intent(in), optional :: &
      IgnorabilityOption, &
      ProcessRankOption, &
      nLeadingBlankLinesOption

    logical(KL) :: &
      AbortShow
    
    call PrepareShow( &
           AbortShow, IgnorabilityOption, ProcessRankOption, &
           nLeadingBlankLinesOption)

    if(AbortShow) return

    print *, '           ', iArray, ':', Array 
    
  end subroutine ShowRealArray


  subroutine ShowLogical( &
               Boolean, Description, IgnorabilityOption, &
               ProcessRankOption, nLeadingBlankLinesOption)

    !-- Convention on argument order violated because the Number being
    !   "Show"n is more important than the Description.

    logical(KL), intent(in) :: &
      Boolean
    character(*), intent(in) :: &
      Description
    integer(KI), intent(in), optional :: &
      IgnorabilityOption, &
      ProcessRankOption, &
      nLeadingBlankLinesOption
      
    logical(KL) :: &
      AbortShow
    
    call PrepareShow( &
           AbortShow, IgnorabilityOption, ProcessRankOption, &
           nLeadingBlankLinesOption)

    if(AbortShow) return

    print '(a35,a3,l10)', trim(Description), '  =', Boolean

  end subroutine ShowLogical
  
  
  subroutine ShowCharacter( &
               String, Description, IgnorabilityOption, ProcessRankOption, &
               nLeadingBlankLinesOption)

    !-- Must agree with Basics/Display/ShowCharacter_Interface.

    character(*), intent(in) :: &
      String, &
      Description
    integer(KI), intent(in), optional :: &
      IgnorabilityOption, &
      ProcessRankOption, &
      nLeadingBlankLinesOption

    logical(KL) :: &
      AbortShow
    
    call PrepareShow( &
           AbortShow, IgnorabilityOption, ProcessRankOption, &
           nLeadingBlankLinesOption)

    if(AbortShow) return

    print '(a35,a,a)', trim(Description), '  =  ', trim(String)

  end subroutine ShowCharacter


  subroutine ShowMessage( &
               Message, IgnorabilityOption, ProcessRankOption, &
               nLeadingBlankLinesOption)

    !-- Must agree with Basics/Display/ShowMessage_Interface.

    character(*), intent(in) :: &
      Message
    integer(KI), intent(in), optional :: &
      IgnorabilityOption, &
      ProcessRankOption, &
      nLeadingBlankLinesOption

    integer(KI) :: &
      iLabel, &
      oChar, &
      lLabel, &
      Ignorability
    logical(KL) :: &
      AbortShow
    character(LL) :: &
      Label
    
    call PrepareShow( &
           AbortShow, IgnorabilityOption, ProcessRankOption, &
           nLeadingBlankLinesOption)

    if(AbortShow) return
    
    Ignorability = CONSOLE_INFO_1
    if(present(IgnorabilityOption)) Ignorability = IgnorabilityOption 

    oChar = 2*(Ignorability-1)
    lLabel = maxval((/ (len_trim(CONSOLE_LABEL(iLabel)), &
                       iLabel = 1, size(CONSOLE_LABEL) ) /)) 
    Label = ''
    Label(oChar+1:oChar+lLabel+2) &
      = trim(CONSOLE_LABEL(Ignorability)) // ': '

    print *, trim(Label) // ' ' // Message

  end subroutine ShowMessage


  subroutine PrepareShow( &
               AbortShow, IgnorabilityOption, ProcessRankOption, &
               nLeadingBlankLinesOption)

    logical(KL), intent(out) :: &
      AbortShow
    integer(KI), intent(in), optional :: &
      IgnorabilityOption, &
      ProcessRankOption, &
      nLeadingBlankLinesOption

    integer(KI) :: &
      iLine, &
      Ignorability

    Ignorability = CONSOLE_INFO_1
    if(present(IgnorabilityOption)) Ignorability = IgnorabilityOption 

    AbortShow = .false.
    
    if(CONSOLE%Mute)then
      AbortShow = .true.
      return
    end if

    if(Ignorability > CONSOLE%Verbosity)then
      AbortShow = .true.
      return
    end if

    if(Ignorability > CONSOLE_WARNING)then
      if(present(ProcessRankOption))then
        if(ProcessRankOption /= CONSOLE%DisplayRank)then
          AbortShow = .true.
          return
        end if
      else 
        if(CONSOLE%ProcessRank /= CONSOLE%DisplayRank)then
          AbortShow = .true.
          return
        end if
      end if
    end if

    if(present(nLeadingBlankLinesOption))then
      do iLine = 1, nLeadingBlankLinesOption
        print *
      end do
    end if
    
  end subroutine PrepareShow


end module Show_Command
