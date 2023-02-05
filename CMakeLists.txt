cmake_minimum_required(VERSION 3.20)

project(fortran-workout-log Fortran)

find_package(jsonfortran-${CMAKE_Fortran_COMPILER_ID} 8.3.0 REQUIRED )

add_library(workout STATIC workout.F90 workout_types.F90)
target_link_libraries(workout PUBLIC jsonfortran-gnu::jsonfortran-static)

add_executable(main main.F90)

target_link_libraries(main PRIVATE workout)

include(CTest)
add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} --verbose)
add_dependencies(check main)

add_test(NAME run
    COMMAND $<TARGET_FILE:main>)
