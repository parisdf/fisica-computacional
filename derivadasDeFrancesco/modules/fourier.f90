module fourier
  implicit none
 contains

  subroutine interpolar_fourier(x, y, n, xout, yout, m)
    real(8), intent(in) :: x(:), y(:)
    integer, intent(in) :: n, m
    real(8), intent(in) :: xout(m)
    real(8), intent(out) :: yout(m)
    real(8) :: L, a0, ak, bk, suma
    integer :: i, j, k
    real :: pi
    pi = 4.0 * atan(1.0)

    L = (x(n) - x(1)) / 2.0
    a0 = sum(y) / n

    do i = 1, m
      suma = a0
      do k = 1, 10
        ak = 0.0
        bk = 0.0
        do j = 1, n
          ak = ak + y(j) * cos(k * pi * (x(j) - x(1)) / L)
          bk = bk + y(j) * sin(k * pi * (x(j) - x(1)) / L)
        end do
        ak = 2.0 * ak / n
        bk = 2.0 * bk / n
        suma = suma + ak * cos(k * pi * (xout(i) - x(1)) / L) + bk * sin(k * pi * (xout(i) - x(1)) / L)
      end do
      yout(i) = suma
    end do
  end subroutine interpolar_fourier 
end module fourier

