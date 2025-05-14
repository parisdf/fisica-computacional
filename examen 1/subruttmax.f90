subroutine maximat(datos, meses, n, tempmaxima, mestempmaxima)

        
        implicit none
        real, dimension(n, 5), intent(in) :: datos
        character(len=20), dimension(n), intent(in) :: meses
        integer, intent(in) :: n
        real, intent(out) :: tempmaxima
        character(len=20), intent(out) :: mestempmaxima
        integer :: i

        tempmaxima = datos(1, 2)
        mestempmaxima = meses(1)

        do i = 1, n
                if (datos(i, 2) > tempmaxima) then
                        tempmaxima = datos(i, 2)
                        mestempmaxima = meses(i)
                endif
        end do
end subroutine maximat
        


                
