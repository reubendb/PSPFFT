!-- Define commonly used KIND constants 

module KIND_Singleton

  implicit none
  private

  integer(kind(1)), public, parameter :: &
    KIND_INTEGER     = kind(1), &
    KIND_INTEGER_BIG = selected_int_kind(15), &
    KIND_REAL        = kind(1.0d0), &
    KIND_COMPLEX     = kind((1.0_KIND_REAL,1.0_KIND_REAL)), &
    KIND_LOGICAL     = kind(.true.)

end module KIND_Singleton
