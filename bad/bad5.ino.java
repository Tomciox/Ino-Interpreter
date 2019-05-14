// za mało argumentów funkcji (lub za dużo)

int f(int a, int b) {
	printInt(a, b) ;
}

int main() {
	f(1);
	// f(1, 2, 3);
	return 0;
}