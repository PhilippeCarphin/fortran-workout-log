PROGRAM main
    use iso_fortran_env
    use workout_history
    implicit none
    integer :: ierr
    character(len=4096) :: home_dir
    integer :: length
    TYPE(WorkoutHistory) :: wh

    call get_environment_variable("HOME", value=home_dir, length=length, status=ierr)
    if(ierr .ne. 0) then
        write(error_unit,*) "Error getting HOME get_environment_variable"
        stop 1
    endif

    call load_workout_history_file(home_dir(1:length)//"/.workout_data.json", wh, ierr)
    if(ierr .ne. 0) then
        write(error_unit,*) "Error loading file"
        stop 1
    endif
    call print_workout_history(wh)

END
