module workout_history
    use json_module
    use, intrinsic :: iso_fortran_env
    use workout_types
    implicit none

    contains

        subroutine load_workout_history_file(filename, wh, ierr)
            character(len=*) :: filename
            type(workouthistory) :: wh
            integer :: ierr

            type(json_value), pointer :: jwh
            type(json_file) :: jfile

            call jfile%initialize()
            if (jfile%failed()) then
                call jfile%print_error_message(error_unit)
                ierr = 1
                return
            endif

            call jfile%load(filename)
            if (jfile%failed()) then
                call jfile%print_error_message(error_unit)
                ierr = 1
                return
            endif

            call jfile%get(jwh)
            if (jfile%failed()) then
                call jfile%print_error_message(error_unit)
                ierr = 1
                return
            endif

            call parse_workout_history(jwh, wh, ierr)
            if(ierr .ne. 0) then
                write(error_unit,*) "could not parse workout history"
                ierr = 1
                return
            endif
        end subroutine

        subroutine parse_workout_history(jwh, wh, ierr)
            type(json_value), pointer :: jwh
            type(workouthistory) :: wh
            integer, intent(out) :: ierr

            type(json_core) :: jcore
            type(json_value), pointer :: jws, jw
            integer :: nb_workouts, iw
            logical :: found

            call jcore%get(jwh, 'workouts', jws, found)
            if(.not. found) then
                write(error_unit, *) "parse_workout_history(): workouts not found"
                ierr = 1
                return
            endif

            call jcore%info(jws, n_children=nb_workouts)
            allocate(wh%workouts(1:nb_workouts))
            do iw=1,nb_workouts
                call jcore%get_child(jws, iw, jw, found)
                call parse_workout(jw, wh%workouts(iw), ierr)
                if(ierr .ne. 0) then
                    write(error_unit,*) "parse_workout_history() error parsing workout #", iw
                    deallocate(wh%workouts)
                    ierr = 1
                    return
                endif
            enddo
            ierr = 0
        end subroutine

        subroutine parse_workout(jw, w, ierr)

            type(json_value), pointer :: jw
            type(workout) :: w
            integer :: ierr

            type(json_core) :: jcore
            type(json_value), pointer :: jwinfo, jexcs, jexc
            logical :: found
            integer :: iexc, nb_excs

            call jcore%get(jw, 'info', jwinfo, found)
            if(.not. found) then
                write(error_unit,*) "parse_workout_test(): info not found"
                ierr = 1
                return
            endif

            call parse_workout_info(jwinfo, w%info, ierr)
            if(ierr .ne. 0) then
                write(error_unit,*) "failure in parse_workout_info()"
                ierr = 1
                return
            endif

            call jcore%get(jw, 'exercises', jexcs, found)
            if(.not. found) then
                write(error_unit,*) "parse_workout_test(): exercises not found"
                ierr = 1
                return
            endif


            call jcore%info(jexcs, n_children=nb_excs)
            allocate(w%exercises(1:nb_excs))
            do iexc = 1, nb_excs
                call jcore%get_child(jexcs, iexc, jexc, found)
                call parse_exercise(jexc, w%exercises(iexc), ierr)
                if(ierr .ne. 0) then
                    write(error_unit,*) "parsing exercise #", iexc, "failed"
                    deallocate(w%exercises)
                    ierr = 1
                    return
                endif
            end do
            ierr = 0
        end subroutine

        subroutine parse_workout_info(jwinfo, wi, ierr)
            type(json_value), pointer :: jwinfo
            type(workoutinfo) :: wi
            integer, intent(out) :: ierr

            type(json_core) :: jcore
            logical :: found

            call jcore%get(jwinfo, 'date', wi%date, found)
            if(.not. found) then
                write(error_unit,*) "parse_workout_info(): date not found"
                ierr = 1
                return
            endif

            call jcore%get(jwinfo, 'main_group', wi%main_group, found)
            if(.not. found) then
                write(error_unit,*) "parse_workout_info(): main_group not found"
                ierr = 1
                return
            endif

            ierr = 0
        end subroutine

        subroutine parse_exercise(jexc, exc, ierr)
            type(json_value), pointer :: jexc
            type(exercise) :: exc
            integer, intent(out) :: ierr

            type(json_core) :: jcore
            type(json_value), pointer :: jeinfo, jsets, jset
            logical :: found
            integer :: iset, nb_sets

            call jcore%get(jexc, 'info', jeinfo, found)
            if(.not. found) then
                write(error_unit,*) "parse_exercise(): info not found"
                ierr = 1
                return
            endif

            call parse_exercise_info(jeinfo, exc%info, ierr)
            if(ierr .ne. 0) then
                write(error_unit,*) "parse_exercise(): failure in parse_exercise_info()"
                ierr = 1
                return
            endif

            call jcore%get(jexc, 'sets', jsets, found)
            if(.not. found) then
                write(error_unit,*) "parse_exercise(): sets not found"
                ierr = 1
                return
            endif

            call jcore%info(jsets, n_children=nb_sets)
            allocate(exc%sets(1:nb_sets))
            do iset = 1, nb_sets
                call jcore%get_child(jsets, iset, jset, found)
                call parse_set(jset, exc%sets(iset), ierr)
                if(ierr .ne. 0) then
                    write(error_unit,*) "parsing set #", iset, "failed"
                    deallocate(exc%sets)
                    ierr = 1
                    return
                endif
            end do
        end subroutine

        subroutine parse_exercise_info(jeinfo, ei, ierr)
            type(json_value), pointer :: jeinfo
            type(exerciseinfo), intent(out) :: ei
            integer, intent(out) :: ierr

            type(json_core) :: jcore
            logical :: found

            call jcore%get(jeinfo, 'name', ei%name, found)
            if( .not. found) then
                write(error_unit,*) "parse_exercise_info(): 'name' not found"
                ierr = 1
                return
            endif

            call jcore%get(jeinfo, 'group', ei%group, found)
            if( .not. found) then
                write(error_unit,*) "parse_exercise_info(): 'group' not found"
                ierr = 1
                return
            endif

            ierr = 0
            return
        end subroutine

        subroutine parse_set(jset, es, ierr)
            type(json_value), intent(in), pointer :: jset
            type(exerciseset), intent(out)        :: es
            integer, intent(out)                  :: ierr

            type(json_core) :: jcore
            logical         :: found

            call jcore%get(jset, 'weight', es%weight, found)
            if( .not. found) then
                write(error_unit,*) "parse_set(): weight not found"
                ierr = 1
                return
            endif

            call jcore%get(jset, 'reps', es%reps, found)
            if( .not. found) then
                write(error_unit,*) "parse_set(): reps not found"
                ierr = 1
                return
            endif

            ierr = 0
        end subroutine

        subroutine print_workout_history(wh)
            type(workouthistory) :: wh
            integer :: iw

            do iw=1,size(wh%workouts, 1)
                call print_workout(wh%workouts(iw))
            enddo
        end subroutine

        subroutine print_workout(w)
            type(workout) :: w

            integer :: iex

            write(error_unit,*) "============================================"

            write(error_unit,*) "workout on "//w%info%date//" for "//w%info%main_group

            do iex=1,size(w%exercises,1)
                call print_exercise(error_unit, w%exercises(iex))
            end do
        end subroutine

        subroutine print_exercise(out_unit, ex)
            integer :: out_unit
            type(exercise) :: ex
            integer :: nb_sets, iset

            write(error_unit, '(a)') "exercise name: "//ex%info%name//" ("//ex%info%group//")"
            do iset = 1, size(ex%sets, 1)
                write(error_unit,*) "weight: ", ex%sets(iset)%weight, "reps", ex%sets(iset)%reps
            end do

        end subroutine

end module
