program esCero
    implicit none
    integer :: numero
    logical :: esCero

    write(*,*) "Ingrese un numero: "
    read(*,*) numero

    if(numero == 0) then
        esCero = .true.
    else 
        esCero = .false.
    end if

    write (*,*) ":", esCero
end program esCero