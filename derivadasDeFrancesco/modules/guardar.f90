module utilidades
        implicit none
contains

 subroutine guardar(nombre, x, y, n)
 
   character(len=*), intent(in) :: nombre
   real(8), intent(in) :: x(:), y(:)
   integer, intent(in) :: n
   integer :: i, unidad
   unidad = 99
   open(unidad, file=nombre, status="replace")
   do i = 1, n
     write(unidad,*) x(i), y(i)
   end do
   close(unidad)
 end subroutine guardar
end module utilidades
