module workout_types

    type :: exerciseset
        real :: weight
        integer :: reps
    end type

    type :: exerciseinfo
        character(len=:), allocatable :: name
        character(len=:), allocatable :: group
    end type

    type :: exercise
        type(exerciseinfo) :: info
        type(exerciseset), dimension(:), allocatable :: sets
    end type

    type :: workoutinfo
        character(len=:), allocatable :: date
        character(len=:), allocatable :: main_group
    end type

    type :: workout
        type(workoutinfo) :: info
        type(exercise), dimension(:), allocatable :: exercises
    end type

    type :: workouthistory
        type(workout), dimension(:), allocatable :: workouts
        type(workout) :: ongoing_workout
        logical :: has_ongoing_workout
    end type

end module
