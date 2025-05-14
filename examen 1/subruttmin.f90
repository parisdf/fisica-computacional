subroutine filtrotempmin(datos, meses, n, m)


        implicit none
        real, dimension(n, 5), intent(in) :: datos
        character(len=20), dimension(n), intent(in) :: meses
        integer, intent(in) :: n
        real, intent(in) :: m
        integer :: i

        print *, "meses donde tmin >", m, ":"

        do i = 1, n
                if (datos(i, 3) > m) then
                        print *, meses(i), datos(i, 3) 
                endif
        end do
end subroutine filtrotempmin


