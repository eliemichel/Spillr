
struct Foo {
	int bar;
	vec2 baz;
};

void main() {
	Foo f;  // requires the lexer hack
	int i = f.bar;
	float x = f.baz.x;
}
