
#include "glsl-parser/parser.h"
#include "glsl-parser/print.h"

#include <cstdio> // EXIT_SUCCESS
#include <iostream>
#include <vector>

using std::cout;
using std::cerr;
using std::endl;

int main(int argc, char **argv) {
	if (argc < 2) {
		cerr << "Usage: UniformSpiller <shader.glsl>" << endl;
		return EXIT_FAILURE;
	}
	const char *filename = argv[1];
	FILE *file = fopen(filename, "r");
	std::vector<char> contents;

	fseek(file, 0, SEEK_END);
    contents.resize(ftell(file));
    fseek(file, 0, SEEK_SET);
    fread(&contents[0], 1, contents.size(), file);
    fclose(file);

	glsl::parser p(&contents[0], filename);
	glsl::astTU *tu = p.parse(glsl::astTU::kFragment);
	if (tu) {
		glsl::printTU(tu);
	} else {
		cerr << p.error() << endl;
	}

	return EXIT_SUCCESS;
}
