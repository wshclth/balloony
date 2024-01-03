program constrain

  use iso_fortran_env

  use standard_atmosphere
  use balloon

  implicit none

  real(real64) :: optimizer_h = 0.01

  ! Target payload size in newtows
  real(real64) :: target_payload_n = 1.0 * 9.81
  real(real64) :: x, fx, dx, fx_ph, fx_mh

  ! Initial conditions
  real(real64) :: volume_at_altitude = 1.0
  real(real64) :: target_altitude = 17000

  ! Results
  real(real64) :: volume_at_surface = 0.0, balloon_radius = 0.0
  
  ! This optimizer, given a target altitude range and the desired payload
  ! size, will solve for initial fill volume and the required burst radius
  ! rating of the balloon.

  ! Find the target volume such that the lift is 0 given the desired payload
  ! size.

  x = volume_at_altitude
  fx = lift(x, target_altitude) - target_payload_n
  do while (abs(fx) >= 1e-8)

    ! Calculate the derivative of f(fill_volume, C) = lift wrt g(fill_volume)
    fx_ph = (lift(x - optimizer_h, target_altitude) - target_payload_n)
    fx_mh = (lift(x + optimizer_h, target_altitude) - target_payload_n)

    ! Derivative of the lift
    dx = (fx_mh - fx_ph) / (2. * optimizer_h)

    ! Compute the new volume at altitude
    x = x - (fx / dx)

    ! Compute the new lift given the new volume
    fx = lift(x, target_altitude) - target_payload_n
  end do

  volume_at_altitude = x

  x = 0.0
  fx = volume_wrt_altitude(target_altitude, x) - volume_at_altitude
  do while (abs(fx) >= 1e-8)

    ! Calculate the derivative of f(fill_volume, C) = lift wrt g(fill_volume)
    fx_ph = (volume_wrt_altitude(target_altitude, x - optimizer_h) - volume_at_altitude)
    fx_mh = (volume_wrt_altitude(target_altitude, x + optimizer_h) - volume_at_altitude)

    ! Derivative of the lift
    dx = (fx_mh - fx_ph) / (2. * optimizer_h)

    ! Compute the new volume at altitude
    x = x - (fx / dx)

    ! Compute the new lift given the new volume
    fx = volume_wrt_altitude(target_altitude, x) - volume_at_altitude
  end do

  volume_at_surface = x

  ! Compute the radius at altitude
  balloon_radius = volume_to_radius(volume_at_altitude)

  print *, "PAYLOAD        ", real(target_payload_n), "N"
  print *, "TARGET ALTITUDE", real(target_altitude), "m"
  print *, ""
  print *, "fill volume  = ", real(volume_at_surface), "m^3"
  print *, "burst radius >=", real(balloon_radius), "m"

end program
