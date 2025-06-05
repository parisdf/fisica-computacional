
module interpolacion
  implicit none
  contains

  function lagrange(xi, yi, n, x) result(fx)
    real(8), intent(in) :: xi(:), yi(:), x
    integer, intent(in) :: n
    real(8) :: fx, l
    integer :: i, j
    fx = 0.0
    do i = 1, n
      l = 1.0
      do j = 1, n
        if (j /= i) l = l * (x - xi(j)) / (xi(i) - xi(j))
      end do
      fx = fx + yi(i) * l
    end do
  end function lagrange

  function newton(xi, yi, n, x) result(fx)
    real(8), intent(in) :: xi(:), yi(:), x
    integer, intent(in) :: n
    real(8) :: fx, coef(n), temp(n)
    integer :: i, j
    temp(1:n) = yi(1:n)


    do j = 2, n
      do i = n, j, -1
        if ((i-j+1)<1) cycle
        temp(i) = (temp(i) - temp(i-1)) / (xi(i) - xi(i-j+1))
      end do
    end do
    coef(1:n) = temp(1:n)


    fx = coef(n)
    do i = n-1, 1, -1
      fx = fx * (x - xi(i)) + coef(i)
    end do
  end function newton
end module interpolacion

