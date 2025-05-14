program notas
        
        implicit none

        integer, parameter  :: n = 3 !cant alum
        character(len=20) :: nombre
        character(len=20), dimension(n) :: nombres
        integer :: i
        real :: nota1(n), nota2(n), nota3(n)
        real :: porc1(n), porc2(n), porc3(n), promedio(n)
        real, parameter :: total = 255.0

        !abrir archivo
        open(unit=10, file='notas.dat', status='old', action='read')

        !leo datos

        do i = 1, n
                read(10, *) nombre, nota1(i), nota2(i), nota3(i)
                nombres(i) = trim(nombre)
        end do
        close(10)

        ! calc proc y prom
        do i = 1, n 
                porc1(i) = 100.0 * nota1(i) / total
                porc2(i) = 100.0 * nota2(i) / total
                porc3(i) = 100.0 * nota3(i) / total
                promedio(i) = (porc1(i) + porc2(i) + porc3(i)) / 3.0
        end do

        !imprim resul en arch
        open(unit=20, file='result.dat', status= 'replace', action='write')
        do i = 1, n
            write(20, '(A,3F8.2,F10.2)') trim(nombres(i)), porc1(i), porc2(i), porc3(i), promedio(i)
        end do
        close(20)
end program notas
