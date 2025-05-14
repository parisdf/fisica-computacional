subroutine estimacion(datos, meses, t, n, tol)

        
        implicit none
        real, dimension(n, 5), intent(in) :: datos
        real, intent(in) :: t, tol
        character(len=20), dimension(n), intent(in) :: meses
        integer, intent(in) :: n
        integer :: i
        
        print*, "meses donde la temperatura se aproxima a ", t
        do i = 1, n
                if (datos(i, 1) >= (t - tol) .and. (datos(i, 1) <= (t + tol))) then
                        print*, meses(i), datos(i, 1)
                endif
        end do
end subroutine estimacion


