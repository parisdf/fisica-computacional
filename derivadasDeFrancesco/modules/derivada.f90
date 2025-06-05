module derivada
  implicit none
  contains

  function derivada_centrada(x, y, n, h) result(dy)
    real(8), intent(in) :: x(:), y(:), h
    integer, intent(in) :: n
    real(8) :: dy(n)
    integer :: i
    dy(1) = (y(2) - y(1)) / h
    dy(n) = (y(n) - y(n-1)) / h
    do i = 2, n-1
      dy(i) = (y(i+1) - y(i-1)) / (2*h)
    end do
  end function derivada_centrada
end module derivada

