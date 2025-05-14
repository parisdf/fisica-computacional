subroutine derivar (f, x, h, d)
        implicit none
        interface
                function f(x)
                        real :: f
                        real, intent(in) :: x
                end function
        end interface

        external :: f
        real, intent(in) :: x, h
        real, intent(out) :: d

        d = (f(x + h) - f(x)) / h
end subroutine
