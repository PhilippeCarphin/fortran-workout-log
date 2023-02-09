module workout_types

    use json_kinds

    type :: exercise_set
        real :: weight
        integer :: reps
    end type

    type :: exercise_info
        character(kind=CK, len=:), allocatable :: name
        character(kind=CK, len=:), allocatable :: group
    end type

    type :: exercise
        type(exercise_info) :: info
        type(exercise_set), dimension(:), allocatable :: sets
    end type

    type :: workout_info
        character(kind=CK, len=:), allocatable :: date
        character(kind=CK, len=:), allocatable :: main_group
    end type

    type :: workout
        type(workout_info) :: info
        type(exercise), dimension(:), allocatable :: exercises
    end type

    type :: workout_history
        type(workout), dimension(:), allocatable :: workouts
        type(workout) :: ongoing_workout
        logical :: has_ongoing_workout
    end type

end module
