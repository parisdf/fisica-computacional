module integracion
  implicit none
contains
  function trapecio_acumulado(x, y, n) result(intf)
    real(8), intent(in) :: x(:), y(:)
    integer, intent(in) :: n
    real(8) :: intf(n)
    integer :: i
    intf(1) = 0.0
    do i = 2, n
      intf(i) = intf(i-1) + 0.5*(x(i) - x(i-1))*(y(i) + y(i-1))
    end do
  end function trapecio_acumulado
end module integracion

