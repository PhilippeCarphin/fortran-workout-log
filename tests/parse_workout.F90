PROGRAM parse_workout_test
    use workout_history
    use json_module
    use, intrinsic :: iso_fortran_env
    implicit none

    integer :: ierr
    LOGICAL :: found

    TYPE(Workout) :: w

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
        write(error_unit,*) "Failure in parse_workout()"
        stop 1
    endif

    call print_workout(w)
    ierr = 0
END PROGRAM

