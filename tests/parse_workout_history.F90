program parse_workout_history_test
    use json_module
    use workout_mod
    use, intrinsic :: iso_fortran_env
    implicit none

    integer :: ierr

    type(workout_history) :: wh

    call load_workout_history_file(FILENAME, wh, ierr)
    if(ierr .ne. 0) then
        write(error_unit,*) "error in load_workout_history_file()"
        stop 1
    endif

    call print_workout_history(error_unit, wh)

    ierr = 0
end program
