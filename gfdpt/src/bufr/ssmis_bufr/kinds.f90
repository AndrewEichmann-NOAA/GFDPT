module kinds
  implicit none
  private


  integer, parameter, public  :: i_byte  = selected_int_kind(1)      ! byte  integer
  integer, parameter, public  :: i_short = selected_int_kind(4)      ! short integer
  integer, parameter, public  :: i_long  = selected_int_kind(8)      ! long  integer
  integer, parameter, private :: llong_t = selected_int_kind(16)     ! llong integer
  integer, parameter, public  :: i_llong = max( llong_t, i_long )

  integer, parameter, public :: num_bytes_for_i_byte  = 1
  integer, parameter, public :: num_bytes_for_i_short = 2
  integer, parameter, public :: num_bytes_for_i_long  = 4
  integer, parameter, public :: num_bytes_for_i_llong = 8

! Define arrays for default definition
  integer, parameter, private :: num_i_kinds = 4
  integer, parameter, dimension( num_i_kinds ), private :: integer_types = (/ &
       i_byte, i_short, i_long,  i_llong  /) 
  integer, parameter, dimension( num_i_kinds ), private :: integer_byte_sizes = (/ &
       num_bytes_for_i_byte, num_bytes_for_i_short, &
       num_bytes_for_i_long, num_bytes_for_i_llong  /)

! Default values
! **** CHANGE THE FOLLOWING TO CHANGE THE DEFAULT INTEGER TYPE KIND ***
  integer, parameter, private :: default_integer = 3  ! 1=byte, 
                                                      ! 2=short, 
                                                      ! 3=long, 
                                                      ! 4=llong
  integer, parameter, public  :: i_kind = integer_types( default_integer )
  integer, parameter, public  :: num_bytes_for_i_kind = &
       integer_byte_sizes( default_integer )


! Real definitions below

! Real types
  integer, parameter, public  :: r_single = selected_real_kind(6)  ! single precision
  integer, parameter, public  :: r_double = selected_real_kind(15) ! double precision
  integer, parameter, private :: quad_t   = selected_real_kind(20) ! quad precision
  integer, parameter, public  :: r_quad   = max( quad_t, r_double )

! Expected 8-bit byte sizes of the real kinds
  integer, parameter, public :: num_bytes_for_r_single = 4
  integer, parameter, public :: num_bytes_for_r_double = 8
  integer, parameter, public :: num_bytes_for_r_quad   = 16

! Define arrays for default definition
  integer, parameter, private :: num_r_kinds = 3
  integer, parameter, dimension( num_r_kinds ), private :: real_kinds = (/ &
       r_single, r_double, r_quad    /) 
  integer, parameter, dimension( num_r_kinds ), private :: real_byte_sizes = (/ &
       num_bytes_for_r_single, num_bytes_for_r_double, &
       num_bytes_for_r_quad    /)


! **** CHANGE THE FOLLOWING TO CHANGE THE DEFAULT REAL TYPE KIND ***
  integer, parameter, private :: default_real = 2  ! 2=double,
  integer, parameter, public  :: r_kind = real_kinds( default_real )
  integer, parameter, public  :: num_bytes_for_r_kind = &
       real_byte_sizes( default_real )


end module kinds
