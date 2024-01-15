! simple_atmosphere module provides simple atmospheric modeling for weather balloon
! predictions of a standard day.
! it's most 
module standard_atmosphere

  use iso_fortran_env

  implicit none

  ! Universal gas constant in J / (mol*K)
  real(real64), private :: Rs = 8.3144598

  ! Acceleration due to gravity in m/s
  real(real64), private :: g0 = 9.80665

  ! Molar mass of the earth's air in kg/mol
  real(real64), private :: M = 0.0289644

  ! Average earth radius in meters
  real(real64), private :: ER = 6.356766E6

contains

  ! meters_to_geopotential_meters converts geometric meters into geopotential
  ! meters.
  ! :param altitude_m: Altitude in geometric meters
  ! :returns: Geopotential meters
  pure function meters_to_geopotential_meters(altitude_m) result(geo_m)
    implicit none

    real(real64), intent(in) :: altitude_m
    real(real64) :: geo_m

    geo_m = ER / (ER + altitude_m) * altitude_m
  end function

  ! altitude_to_g computes the acceleration due to gravity at a given height
  function altitude_to_g(altitude_m) result(g)
     implicit none

     real(real64) :: altitude_m, g

     g = g0 * (ER / (ER + altitude_m)) ** 2.0
  end function

  ! atmospheric_constants provides the constants needed for calculating tempurature
  ! and pressure for the barometric formula of earth.
  ! https://en.wikipedia.org/wiki/Barometric_formula
  ! :param geopotential_altitude: Altitude to return constants for
  ! :returns: Vector of the following constants with types,
  !           hb - height of reference level b in meters
  !           p0 - reference pressure in Pascals
  !           Tb - standard tempurature in Kelvin
  !           Lb - temperature lapse in K/m
  !           (g0 M) / (R L) - the exponent of evaluted constants, unitless
  !           rhob - reference mass density in kg/m^3
  function atmospheric_constants(geopotential_altitude) result(constants)
    implicit none

    real(real64) :: geopotential_altitude
    real(real64), dimension(6) :: constants

    if (geopotential_altitude < 0) then
      stop "geopotential_altitude < 0 (aborting)"
    end if

    if (geopotential_altitude >= 0 .AND. geopotential_altitude < 11000) then
      constants = [0., 101325.0, 288.15, 0.0065, 5.2558, 1.2250]
    else if (geopotential_altitude >= 11000.0 .AND. geopotential_altitude < 20000.0) then
      constants = [11000.0, 22632.10, 216.65, 0.0, 0.0, 0.36391]
    else if (geopotential_altitude >= 20000.0 .AND. geopotential_altitude < 32000.0) then
      constants = [20000.0, 5474.89, 216.65, -0.001, -34.1626, 0.08803]
    else if (geopotential_altitude >= 32000.0 .AND. geopotential_altitude < 47000.0) then
      constants = [32000.0, 868.02, 228.65, -0.0028, -12.2009, 0.01322]
    else if (geopotential_altitude >= 47000.0 .AND. geopotential_altitude < 51000.0) then
      constants = [47000.0, 110.91, 270.65, 0.0, 0.0, 0.00143]
    else if (geopotential_altitude >= 51000.0 .AND. geopotential_altitude < 71000.0) then
      constants = [51000.0, 66.94, 270.65, 0.0028, 12.2009, 0.00086]
    else if (geopotential_altitude >= 71000.0) then
      constants = [71000.0, 3.96, 214.65, 0.002, 17.0813, 0.000064]
    end if 
  end function

  ! pressure_wrt_altitude computes the pressure with respect to altitude in meters.
  ! returns the pressure in pascals
  function pressure_wrt_altitude(altitude_m) result(p)
    implicit none

    real(real64) :: altitude_m
    real(real64) :: p

    real(real64), dimension(6) :: atm_constants
    real(real64) :: hb, p0, Tb, Lb, g0MRL, geo_pot_m

    ! The density formula takes in altitude_m as geopotential altitude
    geo_pot_m = meters_to_geopotential_meters(altitude_m)

    ! Begin by finding the atmospheric constants
    atm_constants = atmospheric_constants(geo_pot_m)

    ! For easability expand the vector into
    hb = atm_constants(1)
    p0 = atm_constants(2)
    Tb = atm_constants(3)
    Lb = atm_constants(4)
    g0MRL = atm_constants(5)

    ! The formula is piecewise based on the Lapse rate of tempurature
    ! for level of the atmosphere
    if (int(hb) .NE. 11000 .AND. int(hb) .NE. 47000) then
      p = p0 * ((Tb - (geo_pot_m - hb) * lb) / Tb) ** g0MRL
    else
      p = p0 * exp((-g0 * M * (geo_pot_m - hb)) / (Rs * Tb))
    end if 
  end function

  ! density_wrt_altitude computes the density with respect to altitude in meters.
  ! returns the density of air in kg/m^3
  function density_wrt_altitude(altitude_m) result(rho)
    implicit none

    real(real64) :: altitude_m
    real(real64) :: rho

    real(real64), dimension(6) :: atm_constants
    real(real64) :: hb, Tb, Lb, g0MRL, rhob, geo_pot_m

    ! The density formula takes in altitude_m as geopotential altitude
    geo_pot_m = meters_to_geopotential_meters(altitude_m)

    ! Begin by finding the atmospheric constants
    atm_constants = atmospheric_constants(geo_pot_m)

    ! For easability expand the vector into
    hb = atm_constants(1)
    Tb = atm_constants(3)
    Lb = atm_constants(4)
    g0MRL = atm_constants(5)
    rhob = atm_constants(6)


    ! The formula is piecewise based on the Lapse rate of tempurature
    ! for level of the atmosphere
    if (int(hb) .NE. 11000 .AND. int(hb) .NE. 47000) then
      rho = rhob * ((Tb - (geo_pot_m - hb) * Lb) / Tb) ** (g0MRL - 1.0)
    else
      rho = rhob * exp((-g0 * M * (geo_pot_m - hb)) / (Rs * Tb))
    end if
  end function

  ! tempurature_wrt_altitude calculates the tempurature with respect to
  ! altitude in meters.
  ! the resulting tempurature is the tempurature in kelvin
  function tempurature_wrt_altitude(altitude_m) result(k)
    implicit none

    ! altitude in meters
    real(real64), intent(in) :: altitude_m

    ! output tempurature in kelvin
    real(real64) :: k

    ! geopotential levels that are valid
    real(real64), dimension(8) :: geopotential_levels = &
            [0.0, 11000.0, 20000.0, 32000.0, 47000.0, 51000.0, 71000.0, 84852.0]

    ! temporary variable to hold the atmospheric constants
    real(real64), dimension(6) :: tmp_atm_constants

    ! search variables for finding the two points of the interpolation
    ! line for tempurature based on the lapse rate
    real(real64) :: geo_pot_m, geopotential_lower_bound, geopotential_upper_bound
    real(real64) :: base_lower_temp, base_upper_temp
    integer :: closest_index
    
    ! for linear interpolation
    real(real64) :: slope, intercept

    ! Convert altitude_m fro geometric meters to geopotential meters
    geo_pot_m = meters_to_geopotential_meters(altitude_m)

    ! Perform bounds checking
    if (geo_pot_m > 84852.0) then
      stop "geo_pot_m > 84852 (aborting)"
    end if 

    if (geo_pot_m < 0) then
      stop "geo_pot_m < 0 (aborting)"
    end if

    ! Find the ranges for interpolation
    closest_index = minloc(abs(geo_pot_m - geopotential_levels), 1)
    if (closest_index == size(geopotential_levels)) then
      geopotential_lower_bound = geopotential_levels(closest_index - 1)
      geopotential_upper_bound = geopotential_levels(closest_index)
    else if (closest_index == 0) then
      geopotential_lower_bound = geopotential_levels(closest_index)
      geopotential_upper_bound = geopotential_levels(closest_index + 1)
    else
      if (geo_pot_m >= geopotential_levels(closest_index - 1) .AND. &
          geo_pot_m < geopotential_levels(closest_index)) then
        geopotential_lower_bound = geopotential_levels(closest_index - 1)
        geopotential_upper_bound = geopotential_levels(closest_index)
      else if (geo_pot_m >= geopotential_levels(closest_index) .AND. &
               geo_pot_m < geopotential_levels(closest_index + 1)) then
        geopotential_lower_bound = geopotential_levels(closest_index)
        geopotential_upper_bound = geopotential_levels(closest_index + 1)
      else
        stop "error in calculating bounds for tempurature calculations"
      end if
    end if

    tmp_atm_constants = atmospheric_constants(geopotential_lower_bound)
    base_lower_temp = tmp_atm_constants(3)

    if (int(geopotential_upper_bound) == 84852) then
      base_upper_temp = 187.15
    else
      tmp_atm_constants = atmospheric_constants(geopotential_upper_bound)
      base_upper_temp = tmp_atm_constants(3)
    end if

    ! the 1976 standard atmosphere says to linearly interpolate between
    ! (geopotential_lower_bound, base_lower_temp)
    ! (geopotential_upper_bound, base_upper_temp)
    ! m = delta y / delta x
    ! b = y - mx
    slope = (base_upper_temp - base_lower_temp) / (geopotential_upper_bound - geopotential_lower_bound)
    intercept = base_lower_temp - (slope * geopotential_lower_bound)

    k = slope * geo_pot_m + intercept
  end function

end module
