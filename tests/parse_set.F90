program test_parse_set
    use workout_history
    use json_module
    use, intrinsic :: iso_fortran_env

    implicit none
    integer :: ierr
    logical :: found

    type(exerciseset) :: es
    type(json_value), pointer :: jset

    type(json_file) :: jfile
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

    call jfile%get(jset)
    if (jfile%failed()) then
        call jfile%print_error_message(error_unit)
        stop 1
    endif

    call parse_set(jset, es, ierr)
    if(ierr .ne. 0) then
        write(error_unit,*) "error parsing json set"
        stop 1
    endif

    write(error_unit,*) "weight: ", es%weight, "reps: ", es%reps

    ierr = 0



end program
