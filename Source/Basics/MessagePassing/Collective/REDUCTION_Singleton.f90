!-- This module defines constants to specify various reduction operations. 

module REDUCTION_Singleton

  use VariableManagement

  implicit none
  private
  
  include 'mpif.h'

  integer(KI), public, parameter :: &
    REDUCTION_MAX                  = MPI_MAX, &
    REDUCTION_MIN                  = MPI_MIN, &
    REDUCTION_SUM                  = MPI_SUM, &
    REDUCTION_PRODUCT              = MPI_PROD, &
    REDUCTION_LOGICAL_AND          = MPI_LAND, &
    REDUCTION_BITWISE_AND          = MPI_BAND, &
    REDUCTION_LOGICAL_OR           = MPI_LOR, &
    REDUCTION_BITWISE_OR           = MPI_BOR, &
    REDUCTION_LOGICAL_EXCLUSIVE_OR = MPI_LXOR, &
    REDUCTION_BITWISE_EXCLUSIVE_OR = MPI_BXOR, &
    REDUCTION_MIN_LOCATION         = MPI_MINLOC, &
    REDUCTION_MAX_LOCATION         = MPI_MAXLOC

end module REDUCTION_Singleton
