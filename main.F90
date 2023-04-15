program main
    use iso_fortran_env
    use workout_mod
    implicit none
    integer :: ierr
    character(len=4096) :: home_dir
    integer :: length
    type(workout_history) :: wh

    call get_environment_variable("HOME", value=home_dir, length=length, status=ierr)
    if(ierr .ne. 0) then
        write(error_unit,*) "error getting home get_environment_variable"
        stop 1
    endif

    call load_workout_history_file(home_dir(1:length)//"/.workout_data.json", wh, ierr)
    if(ierr .ne. 0) then
        write(error_unit,*) "error loading file"
        stop 1
    endif
    call print_workout_history(output_unit, wh)

end
