program Ajuste_de_Curvas
        implicit none
        integer::i,j,k,n,ios,funcion,poli_tipo
        character(len=100)::archivo,archivo_salida
        real*8::a0,a1
        real*8::xx(100),yy(100),x_int,y_int,term
        real*8,allocatable::x(:),y(:)
        real*8,allocatable::L(:)
        real*8,allocatable::DD(:,:)
        
        !Carga de archivo y cuenta-filas
        n=0
        write(*,*)'ingrese el nombre del archivo .dat'
                read(*,*) archivo
                open(10,file=archivo,status='old',iostat=ios)
                if (ios/=0) then !Revision de la existencia del archivo
                        print*, 'Error al abrir el archivo'
                        stop
                end if
                read(10,*) !Salto del encabezado
                do !Calculo de la cantidad de filas del archivo
                        read(10,*,iostat=ios)
                        if (ios/=0) exit
                        n=n+1
                end do
        close(10)
        
        !Tipo de funcion de ajuste
        write(*,*) 'Tipo de funcion'
        write(*,*) '1: Lineal'
        write(*,*) '2: Exponencial'
        write(*,*) '3: Polinomica'
        read(*,*) funcion
         

        !Ajuste Lineal
        if (funcion==1) then
               allocate(x(n),y(n)) !y=y x=x
               print*, size(x)
               
                open(10,file=archivo,status='old')
                        read(10,*) !Salto del encabezado
                        do i=1,n
                                read(10,*) x(i),y(i)
                        end do        
                close(10)         
        
                call RCM(n,x,y,a0,a1) !Calculo de la recta lineal de ajuste
                
                write(*,*) 'Funcion de Ajuste:'
                write(*,*) 'f(x)=',a1,'x +',a0

                !Para dibujar la funcion y = a1 x + a0
                archivo_salida=adjustl(archivo)
                archivo_salida(len_trim(archivo_salida)-3:)='.out'
                open(40,file=archivo_salida,status='unknown')
                        do i=1,100
                                xx(i)=dble(i)
                                yy(i)=a1*xx(i)+a0
                                write(40,*) xx(i), yy(i)
                        end do
                close(40)


        !Ajuste Exponencial
        else if (funcion==2) then
                allocate(x(n),y(n)) !y=log(y) x=x
                open(10,file=archivo,status='old')
                        read(10,*) !Salto del encabezado
                        do i=1,n
                                read(10,*) x(i),y(i)
                                y(i)=log(y(i))
                        end do
                close(10)
                
                call RCM(n,x,y,a0,a1) !Calculo de la recta lineal de ajuste
                
                write(*,*) 'Funcion de Ajuste'
                write(*,*) 'f(x)=',exp(a0),'*e^(',a1,'*x)'

                !Para dibujar la funcion y=be^(mx)
                archivo_salida=adjustl(archivo)
                archivo_salida(len_trim(archivo_salida)-3:)='.out'
                open(40,file=archivo_salida,status='unknown')
                        do i=1,100
                                xx(i)=dble(i)
                                yy(i)=exp(a0+a1*xx(i))
                                write(40,*) xx(i), yy(i)
                        end do
                close(40)


        !Ajuste Polinomico
        else if (funcion==3) then
                print*, 'Valor x de interpolacion'
                read(*,*) x_int
                print*, 'Tipo de Polinomio'
                print*, '1: Polinomio de Lagrange'
                print*, '2: Polinomio de Newton'
                read(*,*) poli_tipo

                !Polinomio de Lagrange
                if(poli_tipo==1) then
                        allocate(x(n),y(n),L(n))
                        open(10,file=archivo,status='old')
                                read(10,*) !Salto del encabezado
                                do i=1,n
                                        read(10,*) x(i),y(i)
                                end do
                        close(10)
                        y_int=0
                        do i=1,n
                                L(i)=1
                                do j=1,n
                                        if(j/=i) then
                                                L(i)=L(i)*((x_int-x(j))/(x(i)-x(j)))
                                        end if
                                end do
                                y_int=y_int+y(i)*L(i)
                        end do
                        print*, 'Valor y de interpolacion para x'
                        print*, y_int
                        stop

                !Polinomio de Newton
                else if(poli_tipo==2) then
                        allocate(x(n),y(n),DD(n,n))
                        open(10,file=archivo,status='old')
                                read(10,*) !Salto de encabezado
                                do i=1,n
                                        read(10,*) x(i),y(i)
                                        DD(i,1)=y(i)
                                end do
                        close(10)
                        do j=2,n !Armado de la Matriz
                                do i=1,n-j+1
                                        DD(i,j)=(DD(i+1,j-1)-DD(i,j-1))/(x(j-1+i)-x(i))
                                end do
                        end do
                        y_int=DD(1,1) !Calculo de y_int
                        do j=2,n
                                term=1
                                do k=1,j-1
                                        term=term*(x_int-x(k))
                                end do
                                y_int=y_int+DD(1,j)*term
                        end do
                        print*, 'Valor y de interpolacion para x'
                        print*, y_int
                        stop

                else
                        print*, 'no existe tal polinomio (todavia)'
                        stop
                end if
                 

        else
                write(*,*) 'No existe tal funcion (todavia)'
                stop
        end if


        write(*,*) 'Graficar con xmgrace ', archivo_salida 

end        
        
subroutine RCM(n,x,y,a0,a1) !Calculo de la recta lineal de ajuste
        implicit none
        integer::i 
        integer,intent(in)::n
        real*8::sx,sy,sxx,sxy
        real*8,intent(in)::x(i),y(i)
        real*8,intent(out)::a0,a1
               
        sx=0
        sy=0
        sxx=0
        sxy=0

        do i=1,n
               sx=sx+x(i)
               sy=sy+y(i)
               sxy=sxy+x(i)*y(i)
               sxx=sxx+x(i)*x(i)
        end do
       
        a1=(dble(n)*sxy-sx*sy)/(dble(n)*sxx-sx*sx)
        a0=(sxx*sy-sxy*sx)/(dble(n)*sxx-sx*sx)
        
end subroutine
