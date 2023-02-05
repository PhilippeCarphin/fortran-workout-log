MODULE workout
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

            call json%get("info", p, found)
            if( .not. found) then
                write(error_unit,*) "Info not found"
                ierr = 1
                return
            endif

            call parse_exercise_info(jcore, p, exercise%info, ierr)
            if(ierr .ne. 0) then
                write(error_unit,*) "Failure in parse_exercise_info()"
                ierr = 1
                return
            endif

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
                call jcore%get_child(jsets, iset, p, found)
                call parse_set(jcore, p, exercise%sets(iset), ierr)
                if(ierr .ne. 0) then
                    write(error_unit,*) "Parsing set #", iset, "FAILED"
                    deallocate(exercise%sets)
                    ierr = 1
                    return
                endif
            end do
            call print_exercise(error_unit, exercise)
            ierr = 0
        END SUBROUTINE

        SUBROUTINE parse_set(jcore, jset, es, ierr)

            implicit none
            type(json_core) :: jcore
            type(json_value), pointer :: jset
            TYPE(ExerciseSet), intent(out) :: es
            integer, intent(out) :: ierr

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

        SUBROUTINE parse_exercise_info(jcore, jeinfo, ei, ierr)
            type(json_core) :: jcore
            type(json_value), pointer :: jeinfo
            TYPE(ExerciseInfo), intent(out) :: ei
            integer, intent(out) :: ierr

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
END
