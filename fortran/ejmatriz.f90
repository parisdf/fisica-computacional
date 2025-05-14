program matriz

        implicit none
        integer :: N, i, j, ios
        real, allocatable :: A(:,:), At(:,:)
        character(len=100) :: archivo

        archivo = 'matriz.dat'

        ! abrir el archivo
        open(unit=10, file=archivo, status='old', action='read', iostat=ios)
        if (ios /= 0) then
                print *, "error al abrir el archivo."
                stop
        end if

        !leer dimension
        read(10, *) N

        ! resevar espacio
        allocate(A(N,N), At(N,N))

        !leer la matriz
        do i = 1, N
                read(10, *) (A(i, j), j = 1, N)
        end do

        close(10)

        !calcular traspuesta

        do i = 1, N
                do j = 1, N
                        At(j, i)= A(i, j)
                end do
        end do

        ! imprimir original

        print *, "matriz original:"
        do i = 1, N
                print *, (A(i, j), j = 1, N)
        end do

        !imprimir traspuesta
        print *, "matriz traspuesta:"
        do i = 1, N
                print *, (At(i, j), j = 1, N)
        end do

end program matriz
