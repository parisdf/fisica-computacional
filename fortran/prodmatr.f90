program prod_matriz

        implicit none
        integer :: N
        real, allocatable :: A(:,:), B(:,:), C(:,:), C_matmul(:,:)
        integer :: i, j

        !sol el tamñ de las matrices

        print *, "introd el tamaño de las matrices cuadradas N:"
        read *, N


        !asignar memoria para las matrices

        allocate(A(N, N), B(N, N), C(N, N), C_matmul(N, N))

        !ing los elem de A

        print *, "introduce los elem de A:"
        do i = 1, N
                do j = 1, N
                        read *, A(i, j)
                end do
        end do

        !ingresar los elem de B
        
        print *, "introduce los elem de B:"
        do i = 1, N
                do j = 1, N
                        read *, B(i, j)
                end do
        end do

        ! llamar a la subrut
        call producto_matrices_manual(A, B, N, C)

        !llamar a la funcion intrinseca matmul para verificar el resultado
        C_matmul = matmul(A, B)

        !mostrar resultado manual

        print *, "prod (A, B) manual:"
        do i = 1, N
                print *, (C(i, j), j = 1, N)
        end do

        ! mostrar resultado usando matmul
        
        print *, "producto de A y B con matmul:"
        do i = 1, N
                print *, (C_matmul(i, j), j = 1, N)
        end do

end program prod_matriz

