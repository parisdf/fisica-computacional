module lector
  implicit none
  contains

  subroutine leer_datos(nombre, x, y, n)
    character(len=*), intent(in) :: nombre
    real(8), intent(out) :: x(:), y(:)
    integer, intent(out) :: n
    integer :: i, estado
    open(unit=10, file=nombre, status='old')
    i = 0
    do
      i = i + 1
      read(10, *, IOSTAT=estado) x(i), y(i)
      if (estado /= 0) exit
    end do
    close(10)
    n = i - 1
  end subroutine leer_datos
end module lector

