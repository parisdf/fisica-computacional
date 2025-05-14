subroutine producto_matrices_manual(A, B, N, C)
        implicit none
        real, dimension(N, N) :: A, B, C
        integer :: N, i, j, k

        ! inic la matriz c con ceros

        C = 0.0


        !calc prod A y B

        do i = 1, N
                do j = 1, N
                        do k = 1, N
                                C(i, j) = C(i, j) + A(i, k) * B(k, j)
                        end do
                end do
        end do
end subroutine producto_matrices_manual

