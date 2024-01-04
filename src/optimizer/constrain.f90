program constrain

  use iso_fortran_env
  use newton_rhapson
  use standard_atmosphere
  use balloon

  implicit none

  ! optimization constants
  real(real64) :: optimizer_h = 0.00001

  ! Target payload size in newtows
  real(real64) :: target_payload_n = 2.0 * 9.81

  ! Initial conditions
  real(real64) :: volume_at_altitude = 1.0
  real(real64) :: target_altitude = 300

  ! Results
  real(real64) :: volume_at_surface = 0.0, balloon_radius = 0.0

  ! Find the target volume such that the lift is 0 given the desired payload
  ! size.
  volume_at_altitude = find_zero(optimize_lift_at_altitude_for_volume, &
    optimizer_h, dble(1E-8), 1.D0)

  ! Find the fill volume such that the volume at altitude gives a lift of zero
  volume_at_surface = find_zero(optimize_volume_at_sealevel_for_volume_at_altitude, &
    optimizer_h, dble(1E-8), 1.D0)

  ! Compute the radius at altitude
  balloon_radius = volume_to_radius(volume_at_altitude)

  print *, "INPUTS"
  print *, "  ", "PAYLOAD        ", real(target_payload_n), "N"
  print *, "  ", "TARGET ALTITUDE", real(target_altitude), "m"

  print *, ""

  print *, "RESULTS"
  print *, " ", "LIFT   @ MSL  =", real(lift(volume_at_surface, 0.D0) - target_payload_n), "N"
  print *, " ", "LIFT   @ TAL  =", &
    real(lift(volume_wrt_altitude(target_altitude, volume_at_surface), target_altitude) - target_payload_n), "N"
  print *, " ", "VOLUME @ TAL  =", real(volume_at_altitude), "m^3"
  print *, " ", "VOLUME @ MSL  =", real(volume_at_surface), "m^3"

  print *, ""

  print *, "REQUIREMENTS"
  print *, " ", "BURST RADIUS >=", real(balloon_radius), "m"

contains

  function optimize_lift_at_altitude_for_volume(x) result(y)
    real(real64) :: x, y
    y = lift(x, target_altitude) - target_payload_n
  end function

  function optimize_volume_at_sealevel_for_volume_at_altitude(x) result(y)
    real(real64) :: x, y
    y = volume_wrt_altitude(target_altitude, x) - volume_at_altitude
  end function

end program
