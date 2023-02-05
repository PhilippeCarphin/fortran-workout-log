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

    TYPE :: WorkoutInfo
        character(len=:), ALLOCATABLE :: date
        character(len=:), ALLOCATABLE :: main_group
    END TYPE

    TYPE :: Workout
        TYPE(WorkoutInfo) :: info
        TYPE(Exercise), dimension(:), ALLOCATABLE :: exercises
    END TYPE

    TYPE :: WorkoutHistory
        TYPE(Workout), dimension(:), ALLOCATABLE :: workouts
        TYPE(Workout) :: ongoing_workout
        LOGICAL :: has_ongoing_workout
    END TYPE

END MODULE
