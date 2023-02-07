cmake_minimum_required(VERSION 3.20)

project(fortran-workout-log Fortran)

find_package(jsonfortran-${CMAKE_Fortran_COMPILER_ID} 8.3.0 REQUIRED )

add_library(workout STATIC workout.F90 workout_types.F90)
target_link_libraries(workout PUBLIC jsonfortran-gnu::jsonfortran-static)

add_executable(main main.F90)

target_link_libraries(main PRIVATE workout)

include(CTest)
add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND})
add_dependencies(check main)

macro(add_parse_test name)
    add_executable(parse_${name} EXCLUDE_FROM_ALL tests/parse_${name}.F90)
    # I used to bake the full of the input JSON file into
    # the test executable this way but it caused a compiler
    # error due to a too-long line so now I used WORKING_DIRECTORY
    # instead.  Because of that, I can't do './parse_set' from the build
    # directory but doing instead 'ctest --verbose -R parse_set' is
    # not that bad
    target_compile_definitions(parse_${name} PRIVATE FILENAME=\"${name}.json\")
    target_link_libraries(parse_${name} PRIVATE workout)
    add_test(NAME parse_${name} COMMAND $<TARGET_FILE:parse_${name}> WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/tests)
    add_dependencies(check parse_${name})
endmacro()

add_parse_test(set)
add_parse_test(exercise)
add_parse_test(workout)
add_parse_test(workout_history)

add_test(NAME run-main COMMAND $<TARGET_FILE:main>)

install(TARGETS main RUNTIME DESTINATION bin)
