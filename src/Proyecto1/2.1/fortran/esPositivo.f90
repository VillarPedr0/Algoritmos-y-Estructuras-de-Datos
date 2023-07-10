program esPositivo
    implicit none
    integer :: numero
    logical :: esPositivo

    write(*,*) "Ingrese un numero: "
    read(*,*) numero

    if(numero > 0) then
        esPositivo = .true.
    else 
        esPositivo = .false.
    end if

    write (*,*) ":", esPositivo
end program esPositivo