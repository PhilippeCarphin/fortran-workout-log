MODULE workout_types

    TYPE :: ExerciseSet
        real :: weight
        integer :: reps
    END TYPE

    TYPE :: ExerciseInfo
        character(len=:), ALLOCATABLE :: name
        character(len=:), ALLOCATABLE :: group
    END TYPE

    TYPE :: Exercise
        TYPE(ExerciseInfo) :: info
        TYPE(ExerciseSet), dimension(:), ALLOCATABLE :: sets
    END TYPE

END MODULE
