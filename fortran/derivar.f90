program derivada
        implicit none
        interface 
                function f1(x)
                        real :: f1
                        real, intent(in) :: x
                end function
        end interface

        real :: x, h, d_aprox
        external :: f_seno, f_x2

        x = 1.0
        h = 0.01

        call derivar(f_seno, x, h, d_aprox)
        print *, "derivada de sin(x) en x = 1 ", d_aprox
        print *, "valor exacto:", cos(x)

        call derivar(f_x2, x, h, d_aprox)
        print *, "derivada de x**2 en x = 1", d_aprox
        print *, "valor exacto:", 2.0*x
end program

! ----------------------------------

function f_seno(x)
        real :: f_seno
        real, intent(in) :: x

        f_seno = sin(x)
end function

function f_x2(x)
        real :: f_x2
        real, intent(in) :: x

        f_x2 = x**2
end function
