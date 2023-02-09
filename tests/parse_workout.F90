program parse_workout_test
    use workout_mod
    use json_module
    use, intrinsic :: iso_fortran_env
    implicit none

    integer :: ierr
    logical :: found

    type(workout) :: w

    type(json_file) :: jfile
    type(json_value), pointer :: jwinfo, jexcs, jw
    type(json_core) :: jcore

    call jfile%initialize()
    if (jfile%failed()) then
        call jfile%print_error_message(error_unit)
        stop 1
    endif

    call jfile%load(FILENAME)
    if (jfile%failed()) then
        call jfile%print_error_message(error_unit)
        stop 1
    endif

    call jfile%get(jw)
    if (jfile%failed()) then
        call jfile%print_error_message(error_unit)
        stop 1
    endif

    call parse_workout(jw, w, ierr)
    if(ierr .ne. 0) then
        write(error_unit,*) "failure in parse_workout()"
        stop 1
    endif

    call print_workout(error_unit, w)
    ierr = 0
end program

