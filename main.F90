PROGRAM main
    use iso_fortran_env
    use workout_history
    integer :: ierr

    write(error_unit,*) "Hello world"
    call workout_hello()
    call parse_set_test(ierr)
    write(error_unit,*) "parse_set_test() :", ierr

    call parse_exercise_test(ierr)
    write(error_unit,*) "parse_exercise_test() :", ierr

    call parse_workout_test(ierr)
    write(error_unit,*) "parse_workout_test() :", ierr

    call parse_workout_history_test(ierr)
    write(error_unit,*) "parse_workout_history_test() :", ierr
END
