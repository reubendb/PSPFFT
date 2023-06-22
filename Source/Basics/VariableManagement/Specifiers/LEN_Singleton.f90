!-- Define commonly use LEN constants for characters 

module LEN_Singleton

  use KIND_Singleton

  implicit none
  private

  integer(KIND_INTEGER), public, parameter :: &
    LEN_NUMBER   = 7, &
    LEN_LABEL    = 31, &
    LEN_FILENAME = 256, &
    LEN_BUFFER   = 1024

end module LEN_Singleton
