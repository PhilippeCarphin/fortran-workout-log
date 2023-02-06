PROGRAM parse_exercise_test
    use workout_history
    use json_module
    use, intrinsic :: iso_fortran_env
    implicit none

    integer :: ierr
    LOGICAL :: found

    type(json_file) :: jfile
    type(json_value), pointer :: jexc
    type(json_core) :: jcore
    TYPE(Exercise) :: exc

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

    call jfile%get(jexc)
    if (jfile%failed()) then
        call jfile%print_error_message(error_unit)
        stop 1
    endif

    call parse_exercise(jexc, exc, ierr)
    if(ierr .ne. 0) then
        write(error_unit,*) "Error parsing exercise"
        stop 1
    endif

    call print_exercise(error_unit, exc)
END PROGRAM
