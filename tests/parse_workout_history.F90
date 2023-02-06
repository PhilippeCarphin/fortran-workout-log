PROGRAM parse_workout_history_test
    use json_module
    use workout_history
    use, intrinsic :: iso_fortran_env
    implicit none

    integer :: ierr

    TYPE(WorkoutHistory) :: wh

    call load_workout_history_file(FILENAME, wh, ierr)
    if(ierr .ne. 0) then
        write(error_unit,*) "Error in load_workout_history_file()"
        stop 1
    endif

    call print_workout_history(wh)

    ierr = 0
END PROGRAM
