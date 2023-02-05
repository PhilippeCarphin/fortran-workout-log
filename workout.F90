MODULE workout
    use json_module
    use, intrinsic :: iso_fortran_env
    use workout_types

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

            type(json_file) :: json
            type(json_value), pointer :: p
            type(json_core) :: jcore

            character(len=*), parameter :: dir = "../"
            character(len=*), parameter :: filename = "exercise_set.json"
            call json%initialize()
            if (json%failed()) then
                call json%print_error_message(error_unit)
                ierr = 1
                return
            endif

            call json%load(dir//filename)
            if (json%failed()) then
                call json%print_error_message(error_unit)
                ierr = 1
                return
            endif

            call json%get_core(jcore)
            call json%get('weight', es%weight, found)
            if( .not. found) then
                write(error_unit,*) "Weight not found"
                ierr = 1
                return
            endif

            call json%get('reps', es%reps, found)
            if( .not. found) then
                write(error_unit,*) "Reps not found"
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

            integer :: nb_sets, iset
            TYPE(ExerciseSet) :: es

            type(json_file) :: json
            type(json_value), pointer :: p
            type(json_value), pointer :: jsets
            type(json_core) :: jcore
            TYPE(Exercise) :: exercise

            character(len=*), parameter :: dir = "../"
            character(len=*), parameter :: filename = "exercise.json"

            call json%initialize()
            if (json%failed()) then
                call json%print_error_message(error_unit)
                ierr = 1
                return
            endif

            call json%load(dir//filename)
            if (json%failed()) then
                call json%print_error_message(error_unit)
                ierr = 1
                return
            endif

            call json%get_core(jcore)

            call json%get('info', p, found)
            if( .not. found) then
                write(error_unit,*) "Info not found"
                ierr = 1
                return
            endif

            call jcore%get(p, 'name', exercise%info%name, found)
            write(error_unit, *) "Exercise Name: ", exercise%info%name

            call jcore%get(p, 'group', exercise%info%group, found)
            write(error_unit, *) "Exercise Group: ", exercise%info%group

            call json%get('sets', jsets, found)
            if( .not. found) then
                write(error_unit,*) "Sets not found"
                ierr = 1
                return
            endif

            call jcore%info(jsets, n_children=nb_sets)
            write(error_unit,*) "Number of sets = ", nb_sets
            ALLOCATE(exercise%sets(1:3))

            do iset = 1, nb_sets
                write(error_unit,*) "SET # ", iset
                call jcore%get_child(jsets, iset, p, found)
                if( .not. found) then
                    write(error_unit,*) "Set #", iset, "NOT FOUND"
                    ierr = 1
                    return
                endif
                call jcore%get(p, 'weight', exercise%sets(iset)%weight, found)
                call jcore%get(p, 'reps', exercise%sets(iset)%reps, found)
                write(error_unit,*) "weight: ", exercise%sets(iset)%weight, "reps", exercise%sets(iset)%reps
            end do



            ierr = 0


        END SUBROUTINE

END
