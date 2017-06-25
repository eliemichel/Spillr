
void main() {
	int i = 0;

	do {
		foo();
		i = bar(i);
	} while (i > 3);
}
