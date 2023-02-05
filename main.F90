PROGRAM main
    use iso_fortran_env
    use workout
    integer :: ierr

    write(error_unit,*) "Hello world"
    call workout_hello()
    call parse_set_test(ierr)
    write(error_unit,*) "parse_set_test() :", ierr

END
