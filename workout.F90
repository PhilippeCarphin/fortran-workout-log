MODULE workout
    use json_module
    use, intrinsic :: iso_fortran_env

    CONTAINS

        SUBROUTINE workout_hello()
            use iso_fortran_env
            write(error_unit,*) "Workout hello"
        END SUBROUTINE

        SUBROUTINE parse_set_test(ierr)
            use workout_types
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

            ! call jcore%get(p, es%weight, found)
            ! if( .not. found) then
            !     write(error_unit,*) "Weight not found"
            !     ierr = 1
            !     return
            ! endif


        END SUBROUTINE

END
