cmake_minimum_required(VERSION 3.10)

project(xcp)

enable_language(CXX)
set(CMAKE_CXX_STANDARD 20)	# Currently ranges feature requires c++latest instead of c++20.
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(MSVC)
	if(CMAKE_CXX_FLAGS MATCHES "/W[0-4]")
		string(REGEX REPLACE "/W[0-4]" "/W4" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
	else()
		set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
	endif()
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}  /Zc:__cplusplus /bigobj -O2")
elseif(CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX)
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic")
endif()

find_package(GTest)
find_package(Threads)
add_executable(xcp
	xcp.cpp
)
target_include_directories(xcp	PRIVATE	.)
target_link_libraries(xcp	${CMAKE_THREAD_LIBS_INIT})
if(DEFINED MSVC)
	target_compile_definitions(xcp	PUBLIC	_CRT_SECURE_NO_WARNINGS  _REGEX_MAX_STACK_COUNT=0)
	target_link_options(xcp	PUBLIC	/STACK:0x400000)
endif()

if(GTest_FOUND)
	target_link_libraries(xcp	GTest::gtest)
	target_compile_definitions(xcp	PUBLIC	xxx_ut)
	add_test(ut
		COMMAND ./xcp -v
	)
endif()
