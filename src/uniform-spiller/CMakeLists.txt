cmake_minimum_required(VERSION 3.1)

set(SRC
	main.cpp
	)

add_executable(UniformSpiller ${SRC})
target_link_libraries(UniformSpiller libGlslParser)
target_compile_features(UniformSpiller PRIVATE cxx_range_for)

