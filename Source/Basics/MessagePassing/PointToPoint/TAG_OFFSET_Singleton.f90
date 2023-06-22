!-- This module defines some offsets for tagging point-to-point messages. 

module TAG_OFFSET_Singleton
  
  use VariableManagement

  implicit none
  private

  integer(KI), public, parameter :: &
    TAG_OFFSET_GHOST          = 0, &
    TAG_OFFSET_REDISTRIBUTION = 100, &
    TAG_OFFSET_PROLONGATION   = 200, &
    TAG_OFFSET_RESTRICTION    = 300

end module TAG_OFFSET_Singleton
