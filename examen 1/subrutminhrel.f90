subroutine minimahrel(datos, meses, n, hrelminima, meshrelminima)

        
        implicit none
        real, dimension(n, 5), intent(in) :: datos
        character(len=20), dimension(n), intent(in) :: meses
        integer, intent(in) :: n
        real, intent(out) :: hrelminima
        character(len=20), intent(out) :: meshrelminima
        integer :: i

        hrelminima = datos(1, 5)
        meshrelminima = meses(1)

        do i = 1, n
                if (datos(i, 5) <  hrelminima) then
                        hrelminima = datos(i, 5)
                        meshrelminima = meses(i)
                endif
        end do
end subroutine minimahrel



