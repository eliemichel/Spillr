Improved Shader Language
========================


Set of tools to reduce boilerplate around GLSL shader writing.


Requirements
------------

(Expressed as Ubuntu packet names)

 * ocaml
 * menhir

Building
--------

### Ocaml parser

	cd ocaml-glsl-parser
	ocamlbuild test.native


### C++ parser

	mkdir build
	cd build
	cmake ..  # or `cmake .. -G "Visual Studio 15 2017 Win64"` or ...
	make  # or `cmake --build .`

