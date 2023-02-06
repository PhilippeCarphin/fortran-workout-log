MODULE workout_history
    use json_module
    use, intrinsic :: iso_fortran_env
    use workout_types
    implicit none

    CONTAINS

        SUBROUTINE workout_hello()
            use iso_fortran_env
            write(error_unit,*) "Workout hello"
        END SUBROUTINE

        SUBROUTINE parse_set_test(ierr)
            implicit none
            integer, intent(out) :: ierr
            LOGICAL :: found

            TYPE(ExerciseSet) :: es
            TYPE(json_value), pointer :: jset

            type(json_file) :: jfile
            type(json_core) :: jcore

            character(len=*), parameter :: dir = "../"
            character(len=*), parameter :: filename = "exercise_set.json"
            call jfile%initialize()
            if (jfile%failed()) then
                call jfile%print_error_message(error_unit)
                ierr = 1
                return
            endif

            call jfile%load(dir//filename)
            if (jfile%failed()) then
                call jfile%print_error_message(error_unit)
                ierr = 1
                return
            endif

            call jfile%get(jset)

            call parse_set(jset, es, ierr)
            if(ierr .ne. 0) then
                write(error_unit,*) "Error parsing json set"
                ierr = 1
                return
            endif

            write(error_unit,*) "weight: ", es%weight, "reps: ", es%reps

            ierr = 0

        END SUBROUTINE

        SUBROUTINE parse_exercise_test(ierr)
            implicit none
            integer, intent(out) :: ierr
            LOGICAL :: found

            type(json_file) :: jfile
            type(json_value), pointer :: jexc
            type(json_core) :: jcore
            TYPE(Exercise) :: exc

            character(len=*), parameter :: dir = "../"
            character(len=*), parameter :: filename = "exercise.json"

            call jfile%initialize()
            if (jfile%failed()) then
                call jfile%print_error_message(error_unit)
                ierr = 1
                return
            endif

            call jfile%load(dir//filename)
            if (jfile%failed()) then
                call jfile%print_error_message(error_unit)
                ierr = 1
                return
            endif

            call jfile%get(jexc)

            call parse_exercise(jexc, exc, ierr)
            if(ierr .ne. 0) then
                write(error_unit,*) "Error parsing exercise"
                ierr = 1
                return
            endif

            call print_exercise(error_unit, exc)
            ierr = 0
        END SUBROUTINE

        SUBROUTINE parse_set(jset, es, ierr)

            implicit none
            type(json_value), pointer :: jset
            TYPE(ExerciseSet), intent(out) :: es
            integer, intent(out) :: ierr

            type(json_core) :: jcore
            LOGICAL :: found

            call jcore%get(jset, 'weight', es%weight, found)
            if( .not. found) then
                write(error_unit,*) "parse_set(): Weight not found"
                ierr = 1
                return
            endif
            call jcore%get(jset, 'reps', es%reps, found)
            if( .not. found) then
                write(error_unit,*) "parse_set(): Reps not found"
                ierr = 1
                return
            endif

            ierr = 0
        END SUBROUTINE

        SUBROUTINE parse_exercise_info(jeinfo, ei, ierr)
            type(json_value), pointer :: jeinfo
            TYPE(ExerciseInfo), intent(out) :: ei
            integer, intent(out) :: ierr

            type(json_core) :: jcore
            LOGICAL :: found

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
        END SUBROUTINE

        SUBROUTINE print_exercise(out_unit, ex)
            integer :: out_unit
            TYPE(Exercise) :: ex
            integer :: nb_sets, iset

            write(error_unit, '(a)') "Exercise Name: "//ex%info%name//" ("//ex%info%group//")"
            do iset = 1, size(ex%sets, 1)
                write(error_unit,*) "weight: ", ex%sets(iset)%weight, "reps", ex%sets(iset)%reps
            end do

        END SUBROUTINE

        SUBROUTINE parse_exercise_sets(jsets, ess, ierr)
            type(json_value), pointer :: jsets
            TYPE(ExerciseSet), dimension(:), ALLOCATABLE :: ess
            integer, intent(out) :: ierr

            integer :: nb_sets, iset
            type(json_core) :: jcore
            type(json_value), pointer :: jset
            LOGICAL :: found

            call jcore%info(jsets, n_children=nb_sets)

            write(error_unit,*) "Number of sets = ", nb_sets
            ALLOCATE(ess(1:nb_sets))

            do iset = 1, nb_sets
                call jcore%get_child(jsets, iset, jset, found)
                call parse_set(jset, ess(iset), ierr)
                if(ierr .ne. 0) then
                    write(error_unit,*) "Parsing set #", iset, "FAILED"
                    deallocate(ess)
                    ierr = 1
                    return
                endif
            end do
            ierr = 0
        end SUBROUTINE

        SUBROUTINE parse_workout_test(ierr)
            integer, intent(out) :: ierr
            LOGICAL :: found

            TYPE(Workout) :: w

            type(json_file) :: jfile
            type(json_value), pointer :: jwinfo, jexcs, jw
            type(json_core) :: jcore

            character(len=*), parameter :: dir = "../"
            character(len=*), parameter :: filename = "workout.json"
            call jfile%initialize()
            if (jfile%failed()) then
                call jfile%print_error_message(error_unit)
                ierr = 1
                return
            endif

            call jfile%load(dir//filename)
            if (jfile%failed()) then
                call jfile%print_error_message(error_unit)
                ierr = 1
                return
            endif

            call jfile%get(jw)
            call parse_workout(jw, w, ierr)
            if(ierr .ne. 0) then
                write(error_unit,*) "Failure in parse_workout()"
                ierr = 1
                return
            endif

            call print_workout(w)
            ierr = 0
        END SUBROUTINE

        SUBROUTINE parse_workout_info(jwinfo, wi, ierr)
            type(json_value), pointer :: jwinfo
            type(WorkoutInfo) :: wi
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
        END SUBROUTINE
        SUBROUTINE parse_exercise(jexc, exc, ierr)
            type(json_value), pointer :: jexc
            TYPE(Exercise) :: exc
            integer, intent(out) :: ierr

            type(json_core) :: jcore
            type(json_value), pointer :: jeinfo, jsets
            LOGICAL :: found
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

            call parse_exercise_sets(jsets, exc%sets, ierr)
            if(ierr .ne. 0) then
                write(error_unit,*) "parse_exercise(): failure in parse_exercise_sets()"
                ierr = 1
                return
            endif

        END SUBROUTINE

        SUBROUTINE parse_exercises(jexcs, excs, ierr)
            type(json_value), pointer :: jexcs
            TYPE(Exercise), dimension(:), ALLOCATABLE :: excs
            integer, intent(out) :: ierr

            integer :: nb_excs, iexc
            type(json_core) :: jcore
            type(json_value), pointer :: jexc
            LOGICAL :: found

            call jcore%info(jexcs, n_children=nb_excs)

            write(error_unit,*) "Number of exercises = ", nb_excs
            ALLOCATE(excs(1:nb_excs))

            do iexc = 1, nb_excs
                call jcore%get_child(jexcs, iexc, jexc, found)
                call parse_exercise(jexc, excs(iexc), ierr)
                if(ierr .ne. 0) then
                    write(error_unit,*) "Parsing exercise #", iexc, "FAILED"
                    deallocate(excs)
                    ierr = 1
                    return
                endif
            end do
            ierr = 0
        end SUBROUTINE
        SUBROUTINE print_workout(w)
            TYPE(Workout) :: w

            integer :: iex

            write(error_unit,*) "============================================"

            write(error_unit,*) "Workout on "//w%info%date//" for "//w%info%main_group

            do iex=1,size(w%exercises,1)
                call print_exercise(error_unit, w%exercises(iex))
            end do
        END SUBROUTINE
        SUBROUTINE parse_workout(jw, w, ierr)

            type(json_value), pointer :: jw
            type(Workout) :: w
            integer :: ierr

            type(json_core) :: jcore
            type(json_value), pointer :: jwinfo, jexcs
            logical :: found

            call jcore%get(jw, 'info', jwinfo, found)
            if(.not. found) then
                write(error_unit,*) "parse_workout_test(): info not found"
                ierr = 1
                return
            endif

            call parse_workout_info(jwinfo, w%info, ierr)
            if(ierr .ne. 0) then
                write(error_unit,*) "Failure in parse_workout_info()"
                ierr = 1
                return
            endif

            call jcore%get(jw, 'exercises', jexcs, found)
            if(.not. found) then
                write(error_unit,*) "parse_workout_test(): exercises not found"
                ierr = 1
                return
            endif

            call parse_exercises(jexcs, w%exercises, ierr)
            if(ierr .ne. 0) then
                write(error_unit,*) "Failure in parse_exercises()"
                ierr = 1
                return
            endif
        END SUBROUTINE
        SUBROUTINE parse_workout_history_test(ierr)
            integer, intent(out) :: ierr

            TYPE(WorkoutHistory) :: wh

            type(json_file) :: jfile
            type(json_value), pointer :: jwh
            type(json_core) :: jcore
            LOGICAL :: found

            character(len=*), parameter :: dir = "../"
            character(len=*), parameter :: filename = "workout_history.json"
            call jfile%initialize()
            if (jfile%failed()) then
                call jfile%print_error_message(error_unit)
                ierr = 1
                return
            endif

            call jfile%load(dir//filename)
            if (jfile%failed()) then
                call jfile%print_error_message(error_unit)
                ierr = 1
                return
            endif

            call jfile%get(jwh)

            call parse_workout_history(jwh, wh, ierr)
            if(ierr .ne. 0) then
                write(error_unit,*) "Error in parse_workout_history"
                ierr = 1
                return
            endif
            call print_workout_history(wh)

            ierr = 0
        END SUBROUTINE
        SUBROUTINE parse_workout_history(jwh, wh, ierr)
            type(json_value), pointer :: jwh
            type(workoutHistory) :: wh
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
                    ierr = 1
                    return
                endif
            enddo
        END SUBROUTINE

        SUBROUTINE print_workout_history(wh)
            TYPE(WorkoutHistory) :: wh
            integer :: iw

            do iw=1,size(wh%workouts, 1)
                call print_workout(wh%workouts(iw))
            enddo
        END SUBROUTINE

END
