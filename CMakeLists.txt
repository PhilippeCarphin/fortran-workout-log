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
    add_executable(parse_${name} tests/parse_${name}.F90)
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
