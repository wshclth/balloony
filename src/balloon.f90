! balloon module provides functions and optimizations that allow us to
! efficiently design high altitude balloons
module balloon

  use iso_fortran_env
  use standard_atmosphere

  implicit none

  ! pV = nRT

  ! The density of hydrogen
  real(real64), private :: DENSITY_HYDROGEN = 0.08375

  ! Universal gas constant
  real(real64), private :: R = 8.31446261815324

  real(real64), private :: PI = 4.D0 * DATAN(1.D0)

contains 

  ! volume_wrt_altitude computes the volume of a balloon assuming no loss and
  ! that the material the balloon is made out of has infinite elasticity at
  ! some altitude above sea level on the surface.
  ! :param altitude_m: The altitude to calculate what the volume of the
  ! balloon is.
  ! :param fill_v: The initial fill volume assumed to be at mean sea level in
  !                m^3
  ! :returns: The volume of the balloon in m^3
  function volume_wrt_altitude(altitude_m, fill_v) result(volume)
    implicit none

    ! Solving for n of the ideal gas law gives us, pV = nRT => n = pV / RT
    ! First we solve for n at sea level. Then we plug that back into the
    ! ideal gas law but rearanged for V.
    ! V = nRT / p

    real(real64) :: altitude_m, fill_v, volume
    real(real64) :: pressure_at_msl, pressure_at_altitude
    real(real64) :: tempurature_at_msl, tempurature_at_altitude

    ! n constant used in ideal gas law
    real(real64) :: n

    pressure_at_msl = pressure_wrt_altitude(0d0)
    tempurature_at_msl = tempurature_wrt_altitude(0d0)

    n = (pressure_at_msl * fill_v) / (R * tempurature_at_msl)

    ! Now use n to calculate the volume at altitude
    pressure_at_altitude = pressure_wrt_altitude(altitude_m)
    tempurature_at_altitude = tempurature_wrt_altitude(altitude_m)

    volume = (n * R * tempurature_at_altitude) / pressure_at_altitude
  end function

  ! volume_to_radius computes the required radius of a sphere to store
  ! the given volume
  function volume_to_radius(volume_m3) result(radius)
    implicit none

    real(real64) :: volume_m3
    real(real64) :: radius

    radius = ((3.0 * volume_m3) / (4.0 * PI)) ** (1.0 / 3.0)
  end function

  ! lift computes the lift of the balloon at a given altitude and returns the
  ! lift in newtons
  function lift(volume_m3, altitude_m) result(lift_newtons)
    implicit none

    real(real64) :: volume_m3, altitude_m, lift_newtons
    real(real64) :: air_density_at_altitude

    air_density_at_altitude = density_wrt_altitude(altitude_m)

    lift_newtons = volume_m3 * (air_density_at_altitude - DENSITY_HYDROGEN) * altitude_to_g(altitude_m)
  end function

end module
