cmake_minimum_required(VERSION 3.10)

project(xcp)

enable_language(CXX)
set(CMAKE_CXX_STANDARD 20)	# Currently ranges feature requires c++latest instead of c++20.
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(POSIX	$<PLATFORM_ID:Linux>)
set(WIN32	$<PLATFORM_ID:Windows>)
set(ANDK	$<BOOL:${CMAKE_ANDROID_API}>)

if(MSVC)
	if(CMAKE_CXX_FLAGS MATCHES "/W[0-4]")
		string(REGEX REPLACE "/W[0-4]" "/W4" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
	else()
		set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
	endif()
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}  /Zc:__cplusplus")
	if(CMAKE_CXX_FLAGS MATCHES "std:c[+][+]20")
		string(REGEX REPLACE "std:c[+][+]+20" "std:c++latest" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
	else()
		set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std:c++latest")
	endif()
elseif(CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX)
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic")
endif()

find_package(GTest)
find_package(Threads)
add_executable(xcp
		xcp.cpp
)
target_include_directories(xcp	PRIVATE .)
if(MSVC)
	target_compile_definitions(xcp	PUBLIC	_CRT_SECURE_NO_WARNINGS)
endif()
if(POSIX)
	message(TRACE "POSIX")
	target_compile_definitions(xcp	PUBLIC	xxx_posix)
elseif(WIN32)
	message(TRACE "Win32")
	target_compile_definitions(xcp	PUBLIC	xxx_win32)
elseif(ANDK)
	message(TRACE "Android")
	target_compile_definitions(xcp	PUBLIC	xxx_andk)
else()
	message(TRACE "Unknown")
endif()
target_link_libraries(xcp	${CMAKE_THREAD_LIBS_INIT})

if(GTest_FOUND)
	target_link_libraries(xcp	GTest::gtest)
	target_compile_definitions(xcp	PUBLIC	xxx_ut)
	add_test(ut
		COMMAND ./xcp -v
	)
endif()
