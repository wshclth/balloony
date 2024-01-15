module newton_rhapson

  use iso_fortran_env

contains

  ! find_zero finds the minimum of easy 1D functions using the newton rhapson
  ! technique
  function find_zero(f, h, tol, x0) result(ans)

    implicit none

    real(real64) :: h, tol, fx, fx_ph, fx_mh, dx, x0, x, ans

    interface
      function f(x) result(y)
        real(8) :: x
        real(8) :: y
      end function
    end interface

    x = x0
    fx = f(x)
    do while (abs(fx) >= tol)
      ! Calculate the derivative of f by fixed points
      fx_ph = f(x - h)
      fx_mh = f(x + h)

      ! Derivative of f
      dx = (fx_mh - fx_ph) / (2. * h)

      ! Compute where to go
      x = x - (fx / dx)

      ! Compute the new value of f
      fx = f(x)
    end do

    ans = x
  end function

end module
